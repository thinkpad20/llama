{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Language.Llama.Types.TypeCheck where
import qualified Prelude as P
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Language.Llama.Types.TypeCheckLib
import qualified Language.Llama.Types.TypeCheckDefaults as Defaults
import Language.Llama.Desugarer.Desugar hiding (testString)
import System.IO.Unsafe (unsafePerformIO)

-- | A wrapper type class, just contains all of the classes we need for things
-- that can be type checked.
class (IsExpr e, Render e, Sourced e) => Valid e where -- no methods
instance Valid DExpr

--------------------------------------------------------------------------
---------------------- Polytype <-> Type functions -----------------------
--------------------------------------------------------------------------

-- | Replaces all quanified variables in the polytype with fresh variables.
instantiate :: Polytype -> TypeChecker Type
instantiate (Polytype vars t) = do
  -- Make a substitution from all variables owned by this polytype
  sub <- fmap compose $ forM (toList vars) $ \v -> oneSub v <$> newvar
  -- Apply the subtitution to the owned type
  return $ sub •> t

-- | Find which variables in the type are completely "owned" by the type 
-- (i.e., are not found in the surrounding environment) and creates a polytype
-- with all of those variables quantified.
generalize :: Type -> TypeChecker Polytype
generalize _type@(Type ctx t) = do
  freeFromEnv <- freevars . _tymaps <$> get
  return $ Polytype ((freevars t <> freevars ctx) \\ freeFromEnv) _type

-- | Takes an instance and replaces all of its variables with fresh variables,
-- so that we don't have 
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

--------------------------------------------------------------------------
---------------- Trait unification and simplification --------------------
--------------------------------------------------------------------------

-- | Returns a new context which is a simplified version of this context.
-- For example, the assertion @{Eq (Maybe a)}@ can be simplified into the
-- assertion @{Eq a}@. The assertion @{Eq Int}@ can become the empty context,
-- since @Int@ implements @Eq@. On the other hand, some assertions are
-- contradictions. For example, the assertion @Eq (Int -> Int)@ is (probably)
-- not true, and should throw an error.
simplify :: Context -> TypeChecker Context
simplify (Context ctx) = mconcat <$> mapM simplify' (S.toList ctx)

simplify' :: Assertion -> TypeChecker Context
simplify' (HasTrait name types) = do
  -- for each instance stored for @name@
    -- for each type in the list of types
      -- try to generate a new context with @match@
      -- if this fails, move on
  -- if no matches
    -- if any of the types are pure type variables, return the types
    -- else assertion failed; throw an error
  putt ["simplifying ", render name, " ", render types]
  getInstancesFor name >>= goInstanceLists
  where
    checkVar = if any isTVar types then return (oneTrait name types)
               else throwError1 $ "No instance of " <> whatsMissing
    goInstanceLists = firstMatchOr checkVar goInstanceList
    goInstanceList = firstMatch goInstance
    goInstance (Instance ctx types') = concatM (match ctx) $ zip types' types
    -- A pretty printer for reporting what caused an error.
    whatsMissing = case normalize types of
      [] -> name
      [t] -> name <> " for type `" <> render t <> "`"
      ts -> name <> " for types " <> commaSep ts
    isTVar = \case {TVar _ -> True; _ -> False}
    match ctx (instType, asrtType) = do
      putt ["Trying to match ", render instType, " with ", render asrtType]
      subs <- unify' instType asrtType
      putt ["got subs: ", render subs]
      simplify (subs •> ctx)

-- | Matches a type vector and a target type variable against a default 
-- instance. For example, if we're trying to find a default for @a@ in the
-- vector @Int a@, and we have a default instance @Int <Int>@, then we'd get
-- back @{a => Int}@. The position of a "slot" in the default instance must
-- correspond to the position of the target type variable.
match1 :: Name -> [BaseType] -> DefaultInstance -> TypeChecker Substitution
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
matchLs target types = firstMatchOr (return zero) $ matchL target types

-- | Takes a trait name and a target type variable, and tries to find a default
-- instance with a slot which matches that variable. If it finds one, it will
-- return a substitution which reflects that. Otherwise, it will return empty.
refineWith :: Name -> Name -> [BaseType] -> TypeChecker Substitution
refineWith trait target types = getDefaults trait >>= matchLs target types

-- | Given an Assertion and a BaseType, sees if the assertion contains any 
-- variables which are not found in the type. If there are any, attempt to
-- find defaults for these variables to disambiguate them.
-- Ex: @{Eq Int a}. b@. The assertion contains @a@, but the base type does not
-- (it only has @b@). Given this, we'd find the default @Eq Int Int@, and 
-- return the substitution @{a => Int}@.
disambiguate1 :: BaseType -> Assertion -> TypeChecker Substitution
disambiguate1 bt (HasTrait trait types) = go $ freelist types where
  go = fmap compose . mapM getSubs
  getSubs var | var `S.member` freevars bt = return zero
              | otherwise = refineWith trait var types

-- | Creates a substitution which removes ambiguity from a type using default 
-- trait instances. For example, if the type were @{IntLiteral b}. a -> Int@, 
-- then @b@ would be ambiguous: it could be any @IntLiteral@ and the exposed 
-- types (@a@ and @Int@) do not restrict it in any way. However, we have a 
-- default trait instance for @IntLit@, which is @Int@. So we disambiguate the 
-- type to @{}. a -> Int@, with the substitution @{b => Int}@.
-- Repeats until there are no further disambiguations to be made.
disambiguate :: Type -> TypeChecker Substitution
disambiguate _type@(Type (Context ctx) bt) = do
  subs <- fmap compose $ mapM (disambiguate1 bt) $ S.toList ctx
  if subs == zero then return subs
  else disambiguate (subs •> _type) >>= \subs' -> return (subs • subs')

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

-- | Get a list of lists of instances for a given trait.
getInstancesFor :: Name -> TypeChecker [[Instance]]
getInstancesFor name = do
  insts <- getInstances
  return $ catMaybes $ fmap (H.lookup name) insts

-- | Gets a list of lists of default instances for a given trait.
getDefaults :: Name -> TypeChecker [[DefaultInstance]]
getDefaults traitName = do
  trmaps <- _trmaps <$> get
  let matching = fmap (H.lookupDefault ([], []) traitName) trmaps
  return $ fmap snd matching


-- | Pushes a new empty environment onto the stack.
push :: TypeChecker ()
push = modify $ \s -> s{_trmaps=mempty:_trmaps s, _tymaps=mempty:_tymaps s}

-- | Pops the top environment off the stack.
pop :: TypeChecker (TypeMap, TraitMap)
pop = do
  typemap:typemaps <- _tymaps <$> get
  traitmap:traitmaps <- _trmaps <$> get
  modify $ \s -> s{_trmaps=traitmaps, _tymaps=typemaps}
  return (typemap, traitmap)

-- | Pushes a new environment with a name and type, does an action, pops.
withContext :: Name -> Type -> TypeChecker a -> TypeChecker a
withContext name _type action = push >> mod >> action <* pop where
  addit (t:ts) = H.insert name (Polytype mempty _type) t : ts
  mod = modify $ \s -> s {_tymaps = addit $ _tymaps s}

-- | The state we start with.
initState :: TCState
initState = TCState
  {_count=0, _tymaps=[Defaults.typeMap], _trmaps=[Defaults.traitMap]}

--------------------------------------------------------------------------
---------------------- Main inferrence functions -------------------------
--------------------------------------------------------------------------

typeof :: (Valid e) => e -> TypeChecker (Substitution, Type)
typeof expr = wrapError expr $ case unExpr expr of
  Int _ -> lit "IntLiteral"
  Float _ -> lit "FloatLiteral"
  String _ -> lit "StrLiteral"
  If c t f -> typeofIf c t f
  Var name -> teLookup name >>= \case
    Nothing -> throwErrorC ["Unknown identifier '", name, "'"]
    Just pt -> only =<< instantiate pt
  Constructor name -> teLookup name >>= \case
    Nothing -> throwErrorC ["Unknown constructor '", name, "'"]
    Just pt -> only =<< instantiate pt
  Typed e (Type ctx t) -> do
    when (ctx /= mempty) $
      throwError1 "Contexts not allowed in type annotations (yet)"
    (s, Type ctx' t') <- typeof e
    s' <- unify (s •> t') t
    ret (s • s') (Type ctx' t')
  Define name expr -> do
    nameT <- newvar
    (subs, Type ctx t) <- withContext name (fromBT nameT) (typeof expr)
    subs' <- unify nameT t
    teStore name =<< generalize ((subs • subs') •> Type ctx nameT)
    ret subs (Type ctx t)
  Then e1 e2 -> do
    (s1, _) <- typeof' e1
    (s2, t2) <- typeof e2
    ret (s1 • s2) t2
  Lambda param body -> do
    paramT <- Type mempty <$> newvar
    (s, t) <- withContext param paramT $ do
      (s, Type ctx t) <- typeof body
      ctx' <- simplify ctx
      ret s (paramT ==> Type ctx' t)
    s' <- disambiguate t
    ret s' t
  Apply e1 e2 -> do
    (subs1, Type ctx1 t1) <- typeof' e1
    (subs2, Type ctx2 t2) <- typeof e2
    tResult <- newvar
    subs3 <- unify t1 (t2 ==> tResult)
    ret (compose [subs1, subs2, subs3]) (Type (ctx1 <> ctx2) tResult)
  _ -> throwErrorC ["Can't type expression ", render expr]
  where only t = return (zero, t)
        ret s t = return (s, s •> t)
        lit name = newvar >>= \tv -> only $ Type (oneTrait name [tv]) tv

-- | Typing an @if@ statement is a bit involved so we split it off here.
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

--------------------------------------------------------------------------
------------------------- Misc utility functions -------------------------
--------------------------------------------------------------------------

wrapError :: (Render e, Sourced e) => e -> TypeChecker a -> TypeChecker a
wrapError e = flip catchError $ \(ErrorList el) -> sourceError e el

-- | Adds the source position to an error message.
sourceError :: (Render e, Sourced e) => e -> [Text] -> TypeChecker a
sourceError e msgs = throwErrorC $
  msgs <> [". In expression `", render e, "` ", render (source e)]

-- A singleton assertion using only type variables.
hasTrait :: Name -> [Name] -> Assertion
hasTrait name = HasTrait name . fmap TVar

-- | Useful when iterating over a list with a possibility of failure. Stops
-- iterating as soon as it successfully applies the function to an element.
firstMatch :: (t -> TypeChecker a) -> [t] -> TypeChecker a
firstMatch _ [] = throwError1 "No match"
firstMatch func (x:xs) = func x `catchError` \_ -> firstMatch func xs

-- | Similar to @firstMatch@, except that it returns a default if no match.
firstMatchOr :: TypeChecker a -> (t -> TypeChecker a) -> [t] ->TypeChecker a
firstMatchOr def func list = firstMatch func list `catchError` \_ -> def

-- Turns [a, b, c] into "a, b and c"
commaSep :: Render a => [a] -> Text
commaSep = \case
  [] -> ""
  [t] -> rndr t
  [t1, t2] -> rndr t1 <> " and " <> rndr t2
  (t:ts) -> rndr t <> ", " <> commaSep ts
  where rndr x = "`" <> render x <> "`"

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

test1 = Type (oneTrait' "IntLiteral" ["a"]) (TVar "b")
test2 = Type ctx (TVar "b") where
  ctx = oneTrait' "IntLiteral" ["a"] <> oneTrait' "Add" ["a", "b", "c"]
test3 = Type ctx (TVar "tx" ==> "()") where
  ctx = oneTrait' "IntLiteral" ["a"] <> oneTrait "Add" [TVar "a", TVar "tx", "Int"]

tst t = runTyping $ fmap render $ do
  putt ["input: ", render t]
  subs <- disambiguate t
  return $ subs •> t
