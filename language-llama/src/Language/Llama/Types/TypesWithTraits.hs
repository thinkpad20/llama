{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
module Language.Llama.Types.TypesWithTraits where
import qualified Prelude as P
import Prelude (Show)
import Data.Set (Set, singleton, (\\))
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Language.Llama.Common.Common hiding (singleton)
import Language.Llama.Common.AST
import Language.Llama.Parser.Parser (Sourced(..))
import Language.Llama.Desugarer.Desugar hiding (testString)
import System.IO.Unsafe (unsafePerformIO)

type TypeMap = Map Name Polytype
type TraitMap = Map Name (Set [BaseType])
newtype TypeEnv = TypeEnv [(TypeMap, TraitMap)] deriving (Show, Eq)
newtype Substitution = Substitution (Map Name BaseType) deriving (Show, Eq)
data TCState = TCState {_count :: Int, _env :: TypeEnv}
type TypeChecker = ErrorT ErrorList (StateT TCState IO)

class Normalize t where
  normalize :: t -> t
  normalize = normalizeWith ("a", mempty)
  normalizeWith :: (Text, HashMap Name Name) -> t -> t

normalizeLoop :: BaseType -> State (Text, HashMap Name Name) BaseType
normalizeLoop type_ = case type_ of
  TVar name -> do
    (name', mapping) <- get
    case H.lookup name mapping of
      Nothing -> do modify (\(n, m) -> (next n, H.insert name name' m))
                    return (TVar name')
      Just n -> return (TVar n)
  TApply a b -> TApply <$> normalizeLoop a <*> normalizeLoop b
  _ -> return type_
  where
    next name = case T.last name of
      c | c < 'z' -> T.init name `T.snoc` succ c
        | True    -> name `T.snoc` 'a'

instance Normalize BaseType where
  normalizeWith state t = evalState (normalizeLoop t) state

instance Normalize Type where
  normalizeWith state (Type ctx t) = Type ctx' t' where
    (ctx', state') = runState (go ctx) state
    t' = normalizeWith state' t
    go :: Context -> State (Text, HashMap Name Name) Context
    go (Context ctx) = do
      let asserts = S.toList ctx
      asserts' <- forM asserts $ \case
        HasTrait n ts -> HasTrait n <$> mapM normalizeLoop ts
      return (Context $ S.fromList asserts')

instance Zero Substitution where
  zero = Substitution mempty

-- | Composes two substitutions. Newer substitution should go second.
Substitution s1 • Substitution s2 = Substitution (s1' <> s2) where
  s1' = fmap (substitute $ Substitution s2) s1

compose :: [Substitution] -> Substitution
compose = foldl' (•) zero

class CanApply a where apply :: a -> a -> a
instance CanApply BaseType where
  apply = TApply
instance CanApply Type where
  apply (Type ctx1 t1) (Type ctx2 t2) = Type (ctx1 <> ctx2) (apply t1 t2)
instance CanApply Polytype where
  apply (Polytype vs1 t1) (Polytype vs2 t2) =
    Polytype (vs1 <> vs2) (apply t1 t2)

class FreeVars a where freevars :: a -> Set Name
instance FreeVars BaseType where
  freevars = \case
    TVar n -> singleton n
    TConst _ -> mempty
    TApply t1 t2 -> freevars t1 <> freevars t2
instance FreeVars Assertion where
  freevars (HasTrait _ types) = freevars types
instance FreeVars Context where
  freevars (Context ctx) = freevars $ toList ctx
instance FreeVars Type where
  freevars (Type ctx t) = freevars ctx <> freevars t
instance FreeVars Polytype where
  freevars (Polytype vars t) = freevars t \\ vars
instance FreeVars TypeEnv where
  freevars (TypeEnv env) = mconcat $ fmap (freevars . H.elems . fst) env
instance FreeVars a => FreeVars [a] where
  freevars = mconcat . fmap freevars

class Substitutable a where substitute :: Substitution -> a -> a
instance Substitutable BaseType where
  substitute subs@(Substitution s) t = case t of
    TVar n | member n s -> s H.! n
    TApply t1 t2 -> TApply (substitute subs t1) (substitute subs t2)
    _ -> t
instance Substitutable Assertion where
  substitute subs (HasTrait n ts) = HasTrait n $ fmap (substitute subs) ts
instance Substitutable Context where
  substitute subs (Context ctx) = Context $ S.map (substitute subs) ctx
instance Substitutable Type where
  substitute subs (Type ctx t) = Type (s ctx) (s t) where s = substitute subs
instance Substitutable Polytype where
  substitute subs (Polytype vars t) = (Polytype vars (s t)) where
    s = substitute (S.foldr' remove subs vars)
instance Substitutable TypeEnv where
  substitute subs (TypeEnv te) = TypeEnv $ fmap go te where
    go (tymap, trmap) = (fmap (substitute subs) tymap, trmap)

generalize :: Type -> TypeChecker Polytype
generalize _type@(Type ctx t) = do
  freeFromEnv <- freevars . _env <$> get
  return $ Polytype ((freevars t <> freevars ctx) \\ freeFromEnv) _type

oneSub :: Name -> BaseType -> Substitution
oneSub n = Substitution . H.singleton n

-- | Takes and returns an integer to denote where to begin renaming from.
instantiate :: Polytype -> TypeChecker Type
instantiate (Polytype vars t) = do
  -- Make a substitution from all variables owned by this polytype
  sub <- fmap compose $ forM (toList vars) $ \v -> oneSub v <$> newvar
  -- Apply the subtitution to the owned type
  return $ substitute sub t

remove :: Name -> Substitution -> Substitution
remove n (Substitution s) = Substitution (delete n s)

(==>) :: (IsString a, CanApply a) => a -> a -> a
t1 ==> t2 = apply (apply "->" t1) t2

infixr 3 ==>

tuple :: (IsString a, CanApply a) => [a] -> a
tuple ts = do
  let name = "Tuple(" <> P.show (length ts) <> ")"
  foldl' apply (fromString name) ts

unify :: BaseType -> BaseType -> TypeChecker Substitution
unify t1 t2 = case (t1, t2) of
  (TVar a, _) -> return (Substitution (H.singleton a t2))
  (_, TVar a) -> return (Substitution (H.singleton a t1))
  (TConst n1, TConst n2) | n1 == n2 -> return zero
  (TApply a1 a2, TApply b1 b2) -> do
    s1 <- unify a1 b1
    s2 <- unify (substitute s1 a2) (substitute s1 b2)
    return (s1 • s2)
  (_, _) -> throwErrorC ["Can't unify types ", render t1, " and ", render t2]

checkContext :: Context -> TypeChecker Context
checkContext c@(Context ctx) = mapM_ check ctx *> pure c where
  check :: Assertion -> TypeChecker ()
  check (HasTrait name ts) =
    -- If there are any free variables, we assume it's valid
    if not (S.null $ freevars ts) then return ()
    -- Otherwise, make sure it's in the TraitMap
    else do
      TypeEnv env <- _env <$> get
      search (fmap snd env)
    where
      search [] = throwError1 $ "No instance of " <> whatsMissing
      search (tmap:rest) = case lookup name tmap of
        Just set | S.member ts set -> return ()
        Nothing -> search rest
      whatsMissing = case ts of
        [] -> name
        [t] -> name <> " for type " <> render t
        ts -> name <> " for types " <> commaSep ts
      commaSep = T.intercalate ", " . map render

newvar :: TypeChecker BaseType
newvar = do
  i <- _count <$> get
  modify $ \s -> s {_count = i + 1}
  return $ TVar $ "$t" <> render i

teLookup :: Name -> TypeChecker (Maybe Polytype)
teLookup name = do
  TypeEnv env <- _env <$> get
  go (fmap fst env) where
    go [] = return Nothing
    go (e:rest) = case lookup name e of
      Nothing -> go rest
      Just pt -> return $ Just pt

teLookup1 :: Name -> TypeEnv -> Maybe Polytype
teLookup1 name (TypeEnv []) = Nothing
teLookup1 name (TypeEnv ((e, _):_)) = H.lookup name e

teStore :: Name -> Polytype -> TypeChecker ()
teStore name ptype = do
  TypeEnv ((te, tr):rest) <- _env <$> get
  modify $ \s -> s {_env = TypeEnv $ (H.insert name ptype te, tr):rest}

-- | Applies a substitution to the environment in the monad.
substituteEnv :: Substitution -> TypeChecker ()
substituteEnv subs = do
  env <- _env <$> get
  modify $ \s -> s {_env = substitute subs env}

pushTypeMap :: TypeMap -> TypeEnv -> TypeEnv
pushTypeMap tmap (TypeEnv env) = TypeEnv $ (tmap, mempty) : env

popTE :: TypeEnv -> TypeEnv
popTE (TypeEnv (_:env)) = TypeEnv env

withContext :: Name -> Type -> TypeChecker a -> TypeChecker a
withContext name _type action = do
  let tmap = H.singleton name (Polytype mempty _type)
  modify $ \s -> s {_env = pushTypeMap tmap $ _env s}
  result <- action
  modify $ \s -> s {_env = popTE $ _env s}
  return result

typeof :: (Render e, Sourced e) => e -> TypeChecker (Substitution, Type)
typeof expr = case unExpr expr of
  Var name -> teLookup name >>= \case
    Nothing -> err ["Unknown identifier '", name, "'"]
    Just pt -> only =<< instantiate pt
  Constructor name -> teLookup name >>= \case
    Nothing -> err ["Unknown constructor '", name, "'"]
    Just pt -> only =<< instantiate pt
  Define name expr -> do
    (subs, t) <- typeof expr
    teStore name =<< generalize (substitute subs t)
    return (subs, substitute subs t)
  Int _ -> do
    TVar name <- newvar
    only $ Type (oneTrait "IntLit" [name]) (TVar name)
  Number _ -> only "Number"
  String _ -> only "String"
  If c t f -> typeofIf c t f
  Then e1 e2 -> do
    (s1, _) <- typeof e1
    (s2, t2) <- substituteEnv s1 >> typeof e2
    return (s1 • s2, t2)
  Lambda param body -> do
    paramT <- Type mempty <$> newvar
    (bodyS, bodyT) <- withContext param paramT $ typeof body
    return (bodyS, substitute bodyS (paramT ==> bodyT))
  Apply e1 e2 -> do
    (subs1, Type ctx1 t1) <- typeof e1
    (subs2, Type ctx2 t2) <- substituteEnv subs1 >> typeof e2
    tResult <- newvar
    subs3 <- unify t1 (t2 ==> tResult) `catchError` \(ErrorList el) -> err el
    let allSubs = subs1 • subs2 • subs3
    ctx' <- checkContext $ substitute allSubs (ctx1 <> ctx2)
    return (allSubs, Type ctx' $ substitute allSubs tResult)
  _ -> err ["Can't type expression ", render expr]
  where only t = return (zero, t)
        err = sourceError expr

typeofIf :: (Sourced e, IsExpr e, Render e) =>
            e -> e -> Maybe e -> TypeChecker (Substitution, Type)
typeofIf c t f = do
  (cSubs, Type cCtx cType) <- typeof c
  uSubs1 <- substituteEnv cSubs >> unify cType "Bool"
  (tSubs, Type tCtx tType) <- substituteEnv uSubs1 >> typeof t
  case f of
    Just f -> do
      (fSubs, Type fCtx fType) <- substituteEnv tSubs >> typeof f
      uSubs2 <- substituteEnv fSubs >> unify tType fType
      let subs = compose [cSubs, uSubs1, tSubs, fSubs, uSubs2]
      ctx <- checkContext $ substitute subs (cCtx <> tCtx <> fCtx)
      let _type = substitute subs (Type ctx tType)
      return (subs, _type)
    Nothing -> do
      let subs = compose [cSubs, uSubs1, tSubs]
      ctx <- checkContext $ substitute subs (cCtx <> tCtx)
      let _type = substitute subs (Type ctx (tuple []))
      return (subs, _type)

sourceError :: (Render e, Sourced e) => e -> [Text] -> TypeChecker a
sourceError e msgs = throwErrorC $
  msgs <> [". In expression `", render e, "` ", render (source e)]

hasTrait :: Name -> [Name] -> Assertion
hasTrait name vars = HasTrait name $ fmap TVar vars

oneTrait :: Name -> [Name] -> Context
oneTrait name = Context . S.singleton . hasTrait name

initState :: TCState
initState = TCState {_count = 0, _env = initTypeEnv} where
  initTypeMap = H.fromList [("Z", "Nat"), ("S", "Nat" ==> "Nat"),
                            ("True", "Bool"), ("False", "Bool"),
                            ("_+_", poly ["a", "b", "c"] plusT),
                            ("-_", poly ["a"] negT)]
  (a, b, c) = (TVar "a", TVar "b", TVar "c")
  negT = Type (oneTrait "Negate" ["a"]) (a ==> a)
  plusT = Type (oneTrait "Add" ["a", "b", "c"]) (a ==> b ==> c)
  poly vars = Polytype (S.fromList vars)
  initTraitMap = mempty
  initTypeEnv = TypeEnv [(initTypeMap, initTraitMap)]

putt :: [Text] -> TypeChecker ()
putt = liftIO . putStrLn . unpack . mconcat

runTyping :: TypeChecker a -> Either ErrorList a
runTyping = flip evalStateIO initState . runErrorT where
  evalStateIO s = fst . unsafePerformIO . runStateT s

typeExpr :: (Render e, IsExpr e, Sourced e)
         => e -> Either ErrorList (Substitution, Type)
typeExpr = runTyping . typeof

typeIt' :: String -> Either ErrorList (Substitution, Type)
typeIt' input = case desugarIt input of
  Left err -> Left err
  Right expr -> typeExpr expr

typeIt :: String -> Either ErrorList Type
typeIt input = case desugarIt input of
  Left err -> Left err
  Right expr -> fmap (normalize . snd) $ typeExpr expr

test :: String -> IO ()
test input = case typeIt input of
  Left err -> error $ P.show err
  Right _type -> putStrLn $ unpack $ render _type
