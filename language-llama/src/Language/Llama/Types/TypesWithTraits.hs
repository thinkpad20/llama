{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | A stored trait instance, which might have its own context.
data Instance = Instance Context [BaseType] deriving (Show)
data DefaultEntry = Slot BaseType | Fixed BaseType deriving (Show)
newtype DefaultInstance = DefaultInstance [DefaultEntry]
  deriving (Show)
-- | Stores names that we've typed
type TypeMap = Map Name Polytype
-- | Stores the instances we've made.
type TraitMap = Map Name ([Instance], [DefaultInstance])
-- | A mapping from (type variable) names to BaseTypes.
newtype Substitution = Substitution (Map Name BaseType) deriving (Show, Eq)
-- | Our monadic state. The count is for generating new names.
data TCState = TCState {_count::Int, _tymaps::[TypeMap], _trmaps::[TraitMap]}
-- | The main type checking monad. Ye olde errorT stateT.
type TypeChecker = ErrorT ErrorList (StateT TCState IO)

instance Render Instance where
  render (Instance ctx ts) = render ctx <> ". " <> T.intercalate " " ts' where
    ts' = fmap renderP ts

instance Render DefaultInstance where
  render (DefaultInstance ts) = "default " <> ts' where
    rndr (Slot t) = "<" <> render t <> ">"
    rndr (Fixed t) = render t
    ts' = T.intercalate " " $ fmap rndr ts

instance Render Substitution where
  render (Substitution s) = "{" <> T.intercalate ", " items <> "}" where
    items = fmap (\(f,t) -> render f <> "=>" <> render t) $ H.toList s

-- | A wrapper type class, just contains all of the classes we need for things
-- that can be type checked.
class (IsExpr e, Render e, Sourced e) => Valid e where -- no methods
instance Valid DExpr

class CanApply a where apply :: a -> a -> a
instance CanApply BaseType where
  apply = TApply
instance CanApply Type where
  apply (Type ctx1 t1) (Type ctx2 t2) = Type (ctx1 <> ctx2) (apply t1 t2)
instance CanApply Polytype where
  apply (Polytype vs1 t1) (Polytype vs2 t2) =
    Polytype (vs1 <> vs2) (apply t1 t2)

class FromBaseType t where
  fromBT :: BaseType -> t

instance FromBaseType BaseType where fromBT bt = bt
instance FromBaseType Type where fromBT = Type mempty
instance FromBaseType Polytype where fromBT bt = Polytype (freevars bt) (fromBT bt)

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
instance FreeVars TypeMap where
  freevars = freevars . H.elems
instance FreeVars a => FreeVars [a] where
  freevars = mconcat . fmap freevars

isConstant :: FreeVars a => a -> Bool
isConstant = S.null . freevars

freelist :: FreeVars a => a -> [Name]
freelist = S.toList . freevars

--------------------------------------------------------------------------
------------------------ Type normalization functions --------------------
--------------------------------------------------------------------------

class Normalize t where
  normalize :: t -> t
  normalize = normalizeWith ("a", mempty)
  normalizeWith :: (Text, HashMap Name Name) -> t -> t
  normalizeWith state t = evalState (normalizeS t) state
  normalizeS :: t -> State (Text, HashMap Name Name) t

instance Normalize BaseType where
  normalizeS type_ = case type_ of
    TVar name -> do
      (name', mapping) <- get
      case H.lookup name mapping of
        Nothing -> do modify (\(n, m) -> (next n, H.insert name name' m))
                      return (TVar name')
        Just n -> return (TVar n)
    TApply a b -> TApply <$> normalizeS a <*> normalizeS b
    _ -> return type_
    where
      next name = case T.last name of
        c | c < 'z' -> T.init name `T.snoc` succ c
          | True    -> name `T.snoc` 'a'

instance Normalize Type where
  normalizeS (Type ctx t) = Type <$> normalizeS ctx <*> normalizeS t

instance Normalize Context where
  normalizeS (Context ctx) = do
    let asserts = S.toList ctx
    asserts' <- forM asserts $ \case
      HasTrait n ts -> HasTrait n <$> normalizeS ts
    return (Context $ S.fromList asserts')

instance Normalize a => Normalize [a] where
  normalizeS = mapM normalizeS

--------------------------------------------------------------------------
-------------------------- Type substitutions ----------------------------
--------------------------------------------------------------------------

class Substitutable a where (•>) :: Substitution -> a -> a
instance Substitutable BaseType where
  subs@(Substitution s) •> t = case t of
    TVar n | member n s -> s H.! n
    TApply t1 t2 -> TApply (subs •> t1) (subs •> t2)
    _ -> t
instance Substitutable Assertion where
  subs •> (HasTrait n ts) = HasTrait n $ fmap (subs •>) ts
instance Substitutable Context where
  subs •> (Context ctx) = Context $ S.map (subs •>) ctx
instance Substitutable Type where
  subs •> (Type ctx t) = Type (subs •> ctx) (subs •> t)
instance Substitutable Polytype where
  subs •> (Polytype vars t) = (Polytype vars (s t)) where
    s = (S.foldr' remove subs vars •>)
instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  subs •> (a, b) = (subs •> a, subs •> b)
instance (Substitutable val) => Substitutable (Map key val) where
  subs •> m = fmap (subs •>) m
instance Substitutable a => Substitutable [a] where
  subs •> list = fmap (subs •>) list

instance Zero Substitution where
  zero = Substitution mempty

-- | Composes two substitutions. Newer substitution should go second.
Substitution s1 • Substitution s2 = Substitution (s1' <> s2) where
  s1' = fmap (Substitution s2 •>) s1

compose :: [Substitution] -> Substitution
compose = foldl' (•) zero

oneSub :: Name -> BaseType -> Substitution
oneSub n = Substitution . H.singleton n

remove :: Name -> Substitution -> Substitution
remove n (Substitution s) = Substitution (delete n s)

--------------------------------------------------------------------------
---------------------- Type constructor wrappers -------------------------
--------------------------------------------------------------------------

-- | The function type, which is actually a rank-2 type applied twice.
(==>) :: (IsString a, CanApply a) => a -> a -> a
t1 ==> t2 = apply (apply "->" t1) t2

infixr 3 ==>

-- | A tuple type, a rank-N type where N is the length of the tuple.
tuple :: (IsString a, CanApply a) => [a] -> a
tuple ts = do
  let name = "Tuple(" <> P.show (length ts) <> ")"
  foldl' apply (fromString name) ts

--------------------------------------------------------------------------
------------------------ Polytype-Type functions -------------------------
--------------------------------------------------------------------------

-- | Takes and returns an integer to denote where to begin renaming from.
instantiate :: Polytype -> TypeChecker Type
instantiate (Polytype vars t) = do
  -- Make a substitution from all variables owned by this polytype
  sub <- fmap compose $ forM (toList vars) $ \v -> oneSub v <$> newvar
  -- Apply the subtitution to the owned type
  return $ sub •> t

generalize :: Type -> TypeChecker Polytype
generalize _type@(Type ctx t) = do
  freeFromEnv <- freevars . _tymaps <$> get
  return $ Polytype ((freevars t <> freevars ctx) \\ freeFromEnv) _type

instantiateInstance :: Instance -> TypeChecker Instance
instantiateInstance (Instance context _type) = do
  let vars = S.toList $ freevars context <> freevars _type
  subs <- compose <$> mapM (\v -> oneSub v <$> newvar) vars
  return $ Instance (subs •> context) (subs •> _type)

--------------------------------------------------------------------------
---------------------- Type unification functions ------------------------
--------------------------------------------------------------------------

-- | Given two types, produces a substitution which if applied would make the
-- two types equal. Throws an error if such a substitution is impossible.
unify :: BaseType -> BaseType -> TypeChecker Substitution
unify t1 t2 = case (t1, t2) of
  (TVar a, _) -> return (Substitution (H.singleton a t2))
  (_, TVar a) -> return (Substitution (H.singleton a t1))
  (TConst n1, TConst n2) | n1 == n2 -> return zero
  (TApply a1 a2, TApply b1 b2) -> do
    s1 <- unify a1 b1
    s2 <- unify (s1 •> a2) (s1 •> b2)
    return (s1 • s2)
  (_, _) -> throwErrorC ["Can't unify types ", render t1, " and ", render t2]

-- | Returns a new context which is a simplified version of this context.
-- For example, the assertion `{Eq (Maybe a)}` can be simplified into the
-- assertion `{Eq a}`. The assertion `{Eq Int}` can become the empty context,
-- since `Int` implements `Eq`. On the other hand, some assertions are
-- contradictions. For example, the assertion `Eq (Int -> Int)` is (probably)
-- not true.
simplify :: Context -> TypeChecker Context
simplify (Context ctx) = mconcat <$> mapM simplify' (S.toList ctx)

simplify' :: Assertion -> TypeChecker Context
simplify' (HasTrait name types) = do
  -- for each instance stored for `name`
    -- for each type in the list of types
      -- try to generate a new context with `match`
      -- if this fails, move on
  -- if no matches
    -- if any of the types are pure type variables, return the types
    -- else assertion failed; throw an error
  putt ["simplifying ", render name, " ", render types]
  instanceLists <- getInstancesFor name
  putt ["instances: ", render instanceLists]
  goILs instanceLists
  where
    -- Iterating over a list of instance lists
    goILs [] =
      if any isTVar types then do
        putt ["No matches, but it's a var!"]
        return (oneTrait name types)
      else throwError1 $ "No instance of " <> whatsMissing
    goILs (il:ils) = goIL il `catchError` \_ -> goILs ils
    -- Iterating over a list of instances
    goIL [] = throwError1 "Nothing matched in this instance list"
    goIL (Instance ctx types':is) = do
      res <- goT ctx types'
      putt ["hey, got results ", render res]
      return res
      `catchError` \_ -> goIL is
    -- Iterate over a list of types and concatenate results
    goT :: Context -> [BaseType] -> TypeChecker Context
    goT ctx types' = do
      putt ["trying to match with instance: ", render ctx, " ", render types']
      fmap mconcat $ mapM (match ctx) $ P.zip types' types
    -- A pretty printer for reporting what caused an error.
    whatsMissing = case normalize types of
      [] -> name
      [t] -> name <> " for type " <> render t
      ts -> name <> " for types " <> commaSep ts
    -- Turns [a, b, c] into "a, b and c"
    commaSep [t1, t2] =  rndr t1 <> " and " <> rndr t2
    commaSep (t:ts) = rndr t <> ", " <> commaSep ts
    rndr x = "`" <> render x <> "`"
    isTVar (TVar _) = True
    isTVar _ = False

match :: Context             -- ^ The instance's context
      -> (BaseType           -- ^ The stored instance's type
      ,   BaseType)          -- ^ The type in the assertion
      -> TypeChecker Context -- ^ The new context
match ctx (instType, asrtType) = do
  putt ["Trying to match ", render instType, " with ", render asrtType]
  subs <- unify' instType asrtType
  putt ["got subs: ", render subs]
  simplify (subs •> ctx)

-- | Almost the same as @unify@, but only records a substitution if the left
-- side is a type variable.
unify' :: BaseType -> BaseType -> TypeChecker Substitution
unify' t1 t2 = case (t1, t2) of
  (TVar a, _) -> return $ oneSub a t2
  (TConst a, TConst b) | a == b -> return zero
  (TApply a b, TApply a' b') -> do
    s1 <- unify' a a'
    s2 <- unify' (s1 •> b) (s1 •> b')
    return $ s1 • s2
  (_, _) -> throwErrorC ["Incompatible: ", render t1, ", ", render t2]

-- | Useful when iterating over a list with a possibility of failure. Stops
-- iterating as soon as it successfully applies the function to an element.
firstMatch :: (t -> TypeChecker a) -> [t] -> TypeChecker a
firstMatch _ [] = throwError1 "No match"
firstMatch func (x:xs) = 
  func x `catchError` \_ -> firstMatch func xs

-- | Similar to @firstMatch@, except that it returns a default if no match.
firstMatchOr :: a -> (t -> TypeChecker a) -> [t] ->TypeChecker a
firstMatchOr def func list = firstMatch func list `catchError` \_ -> return def

-- | Matches a type vector and a target type variable against a default 
-- instance. For example, if we're trying to find a default for `a` in the
-- vector `Int a`, and we have a default instance `Int <Int>`, then we'd get
-- back `{a => Int}`.
match1 :: Name -> [BaseType] -> DefaultInstance 
             -> TypeChecker Substitution
match1 target types (DefaultInstance types') = do
  fmap compose $ forM (zip types types') $ \case
    (TVar name, Slot t) | name == target -> return $ oneSub name t
    (t, Fixed t') | not (target `S.member` freevars t) -> unify' t' t
    _ -> throwError1 "No match"

-- | Like @match1@, but operates on a list of default instances. 
-- Returns the first one that matches, or errors out.
matchL :: Name -> [BaseType] -> [DefaultInstance] -> TypeChecker Substitution
matchL target types = firstMatch (match1 target types)

-- | Like @match1@, but operates on a list of lists of default instances.
-- Returns the first one that matches, or returns an empty substitution.
matchLs :: Name -> [BaseType] -> [[DefaultInstance]] -> TypeChecker Substitution
matchLs target types = firstMatchOr zero (matchL target types)

-- | Takes a trait name and a target type variable, and tries to find a default
-- instance with a slot which matches that variable. If it finds one, it will
-- return a substitution which reflects that. Otherwise, it will return empty.
refineWith :: Name -> Name -> [BaseType] -> TypeChecker Substitution
refineWith trait target types = getDefaults trait >>= matchLs target types

-- | Given an Assertion and a BaseType, sees if the assertion contains any 
-- variables which are not found in the type. If there are any, attempt to
-- find defaults for these variables to disambiguate them.
-- Ex: `{Eq Int a}. b`. Given this, we'd find the default `Eq Int Int`, and 
-- return the substitution `{a => Int}`.
disambiguate1 :: BaseType -> Assertion -> TypeChecker Substitution
disambiguate1 bt (HasTrait trait types) = go $ freelist types where
  go = fmap compose . mapM getSubs
  getSubs var | var `S.member` freevars bt = return zero
              | otherwise = refineWith trait var types

-- | Creates a substitution which removes ambiguity from a type using default 
-- trait instances. For example, if the type were `{IntLiteral b}. a -> Int`, 
-- then `b` would be ambiguous: it could be any `IntLiteral` and the exposed 
-- types (`a` and `Int`) do not restrict it in any way. However, we have a 
-- default trait instance for `IntLit`, which is `Int`. So we disambiguate the 
-- type to `{}. a -> Int`, with the substitution `{b => Int}`.
-- Repeats until there are no further disambiguations to be made.
disambiguate :: Type -> TypeChecker Substitution
disambiguate _type@(Type (Context ctx) bt) = do
  subs <- fmap compose $ mapM (disambiguate1 bt) $ S.toList ctx
  if subs == zero then return subs
  else disambiguate (subs •> _type) >>= \subs' -> return (subs • subs')

-- | Gets a list of lists of default instances for a given trait.
getDefaults :: Name -> TypeChecker [[DefaultInstance]]
getDefaults traitName = do
  trmaps <- _trmaps <$> get
  let matching = fmap (H.lookupDefault ([], []) traitName) trmaps
  return $ fmap snd matching


--------------------------------------------------------------------------
------------------------ Environment management --------------------------
--------------------------------------------------------------------------

-- | Produces a fresh type variable.
newvar :: TypeChecker BaseType
newvar = do
  i <- _count <$> get
  modify $ \s -> s {_count = i + 1}
  return $ TVar $ "$t" <> render i

-- | Looks up a variable in the type environment (all available).
teLookup :: Name -> TypeChecker (Maybe Polytype)
teLookup name = _tymaps <$> get >>= go where
  go [] = return Nothing
  go (e:rest) = case lookup name e of
    Nothing -> go rest
    Just pt -> return $ Just pt

-- | Looks up a variable in the local type environment only.
teLookup1 :: Name -> [TypeMap] -> Maybe Polytype
teLookup1 name [] = Nothing
teLookup1 name (tm:_) = H.lookup name tm

-- | Stores the polytype of a variable.
teStore :: Name -> Polytype -> TypeChecker ()
teStore name ptype = do
  tm:rest <- _tymaps <$> get
  modify $ \s -> s {_tymaps = H.insert name ptype tm:rest}

-- | Applies a substitution to the environment in the monad.
substituteEnv :: Substitution -> TypeChecker ()
substituteEnv subs = do
  modify $ \s -> s {_tymaps = subs •> _tymaps s}

-- | Gets all trait instances we have stored.
getTraitMaps :: TypeChecker [TraitMap]
getTraitMaps = _trmaps <$> get

-- | Gets all trait instances we have stored.
getInstances :: TypeChecker [Map Name [Instance]]
getInstances = do
  trmaps <- getTraitMaps
  return $ fmap (fmap fst) trmaps  

getInstancesFor :: Name -> TypeChecker [[Instance]]
getInstancesFor name = do
  insts <- getInstances
  return $ catMaybes $ fmap (H.lookup name) insts

-- | The state we start with.
initState :: TCState
initState = TCState {_count=0, _tymaps=[typeMap], _trmaps=[traitMap]} where
  typeMap = H.fromList [("Z", "Nat"), ("S", "Nat" ==> "Nat"),
                        ("Point", fromBT (a ==> point a)),
                        ("Just", fromBT (a ==> maybe a)),
                        ("Nothing", fromBT (maybe a)),
                        ("True", "Bool"), ("False", "Bool"),
                        ("_+_", poly ["a", "b", "c"] plusT),
                        ("-_", poly ["a"] negT),
                        ("_==_", poly ["a"] eqT)]
  (a, b, c) = (TVar "a", TVar "b", TVar "c")
  eqT = Type (oneTrait' "Eq" ["a", "b"]) (a ==> b ==> "Bool")
  negT = Type (oneTrait' "Negate" ["a"]) (a ==> a)
  plusT = Type (oneTrait' "Add" ["a", "b", "c"]) (a ==> b ==> c)
  poly vars = Polytype (S.fromList vars)
  point = apply "Point"
  maybe = apply "Maybe"
  list = apply "List"
  addInstances = [ Instance mempty ["Int", "Int", "Int"]
                 , Instance mempty ["Nat", "Nat", "Nat"]
                 , Instance (oneTrait' "Add" ["a", "a", "a"]) 
                            [point a, point a, point a]]
  addDefaults = [ DefaultInstance [Fixed "Int", Fixed "Int", Slot "Int"]
                , DefaultInstance [Fixed "Int", Fixed "Float", Slot "Float"]
                , DefaultInstance [Fixed "Float", Fixed "Int", Slot "Float"]
                , DefaultInstance [Fixed "Float", Fixed "Float", Slot "Float"]]
  eqInstances = [ Instance mempty ["Int"]
                , Instance mempty ["Nat"]
                , Instance (oneTrait' "Eq" ["a"]) [maybe a]]
  eqDefaults = [ DefaultInstance [Fixed (TVar "a"), Slot (TVar "a")]
               , DefaultInstance [Slot (TVar "a"), Fixed (TVar "a")]]
  intLitInstances = [ Instance mempty ["Int"]
                    , Instance mempty ["Nat"]
                    , Instance mempty ["Float"]] 
  intLitDefaults = [DefaultInstance [Slot "Int"]]
  strLitInstances = [ Instance mempty ["String"]
                    , Instance mempty ["Char"]]
  strLitDefaults = [DefaultInstance [Slot "String"]]
  traitMap = H.fromList [
      ("Add", (addInstances, addDefaults))
    , ("Eq",  (eqInstances, eqDefaults))
    , ("IntLiteral", (intLitInstances, intLitDefaults))
    , ("StrLiteral", (strLitInstances, strLitDefaults))
    ]

--------------------------------------------------------------------------
---------------------- Main inferrence functions -------------------------
--------------------------------------------------------------------------

typeof :: (Valid e) => e -> TypeChecker (Substitution, Type)
typeof expr = case unExpr expr of
  Int _ -> lit "IntLiteral"
  Float _ -> lit "FloatLiteral"
  String _ -> lit "StrLiteral"
  If c t f -> typeofIf c t f
  Var name -> teLookup name >>= \case
    Nothing -> err ["Unknown identifier '", name, "'"]
    Just pt -> only =<< instantiate pt
  Constructor name -> teLookup name >>= \case
    Nothing -> err ["Unknown constructor '", name, "'"]
    Just pt -> only =<< instantiate pt
  Define name expr -> do
    (subs, t) <- typeof expr
    teStore name =<< generalize (subs •> t)
    ret subs t
  Then e1 e2 -> do
    (s1, _) <- typeof' e1
    (s2, t2) <- typeof e2
    ret (s1 • s2) t2
  Lambda param body -> do
    paramT <- Type mempty <$> newvar
    withContext param paramT $ do
      (s, Type ctx t) <- typeof body
      ctx' <- simplify ctx
      ret s (paramT ==> Type ctx' t)
  Apply e1 e2 -> do
    (subs1, Type ctx1 t1) <- typeof' e1
    (subs2, Type ctx2 t2) <- typeof e2
    tResult <- newvar
    subs3 <- unify t1 (t2 ==> tResult) `catchError` \(ErrorList el) -> err el
    ret (compose [subs1, subs2, subs3]) (Type (ctx1 <> ctx2) tResult)
  _ -> err ["Can't type expression ", render expr]
  where only t = return (zero, t)
        err = sourceError expr
        ret s t = return (s, s •> t)
        lit name = newvar >>= \tv -> only $ Type (oneTrait name [tv]) tv

-- | Typing an `if` statement is a bit involved so we split it off here.
typeofIf :: (Valid e) => e -> e -> Maybe e -> TypeChecker (Substitution, Type)
typeofIf c t f = do
  (cSubs, Type cCtx cType) <- typeof' c
  uSubs1 <- unify cType "Bool"
  (tSubs, Type tCtx tType) <- substituteEnv uSubs1 >> typeof' t
  case f of
    Just f -> do
      (fSubs, Type fCtx fType) <- typeof' f
      uSubs2 <- unify tType fType
      let subs = compose [cSubs, uSubs1, tSubs, fSubs, uSubs2]
      let ctx' = subs •> (cCtx <> tCtx <> fCtx)
      return (subs, subs •> (Type ctx' tType))
    Nothing -> do
      let subs = compose [cSubs, uSubs1, tSubs]
      return (subs, subs •> Type (cCtx <> tCtx) (tuple []))

-- | Gets the type and applies the substitutions to the environment in one go.
typeof' :: (Valid e) => e -> TypeChecker (Substitution, Type)
typeof' expr = typeof expr >>= \(s, t) -> substituteEnv s >> return (s, t)

push :: TypeChecker ()
push = modify $ \s -> s{_trmaps=mempty:_trmaps s, _tymaps=mempty:_tymaps s}

pop :: TypeChecker (TypeMap, TraitMap)
pop = do
  tymap <- head . _tymaps <$> get
  trmap <- head . _trmaps <$> get
  modify $ \s -> s{_trmaps=tail $ _trmaps s, _tymaps=tail $ _tymaps s}
  return (tymap, trmap)

-- | Pushes a new environment with a name and type, does an action, pops.
withContext :: Name -> Type -> TypeChecker a -> TypeChecker a
withContext name _type action = push >> mod >> action <* pop where
  addit (t:ts) = H.insert name (Polytype mempty _type) t : ts
  mod = modify $ \s -> s {_tymaps = addit $ _tymaps s}

-- | Adds the source position to an error message.
sourceError :: (Render e, Sourced e) => e -> [Text] -> TypeChecker a
sourceError e msgs = throwErrorC $
  msgs <> [". In expression `", render e, "` ", render (source e)]

hasTrait :: Name -> [Name] -> Assertion
hasTrait name vars = HasTrait name $ fmap TVar vars

oneTrait' :: Name -> [Name] -> Context
oneTrait' name = Context . S.singleton . hasTrait name

oneTrait :: Name -> [BaseType] -> Context
oneTrait name = Context . S.singleton . HasTrait name

--------------------------------------------------------------------------
---------------------- Running the type checker --------------------------
--------------------------------------------------------------------------

-- | For debugging print statements while we're type checking.
putt :: [Text] -> TypeChecker ()
putt | showLogs = liftIO . putStrLn . unpack . mconcat
     | otherwise = \_ -> return ()

-- | Unwrapping type checking monads.
runTyping :: TypeChecker a -> Either ErrorList a
runTyping = flip evalState' initState . runErrorT where
  evalState' s = fst . unsafePerformIO . runStateT s

-- | Gets the type of an expression.
typeExpr :: (Valid e) => e -> Either ErrorList (Substitution, Type)
typeExpr = runTyping . typeof

-- | Gives the type of an expression, starting by parsing a string.
typeIt :: String -> Either ErrorList (Substitution, Type)
typeIt input = case desugarIt input of
  Left err -> Left err
  Right expr -> typeExpr expr

-- | Tests in the IO monad.
test :: String -> IO ()
test input = case fmap snd $ typeIt input of
  Left err -> error $ P.show err
  Right _type -> putStrLn $ unpack $ render $ normalize _type

showLogs = False
