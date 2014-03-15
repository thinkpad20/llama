{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeChecker ( Typable(..), Typing, TypeTable, TypingState(..)
                   , runTyping, runTypingWith, defaultTypingState
                   , typeIt, unifyIt) where

import Prelude hiding (lookup, log)
import System.IO.Unsafe
import Control.Monad.Error.Class
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Common
import AST
import Parser (grab, grabT)

class Typable a where
  typeOf :: a -> Typing Type

type TypeTable = M.Map Name Type
type NameSpace = [Name]
data TypingState = TypingState { aliases :: M.Map Name Type
                               , nameSpace :: [Name]
                               , nameSpaceTable :: TypeTable
                               , freshName :: Name } deriving (Show)
type Typing = ErrorT ErrorList (StateT TypingState IO)

instance Render TypingState where
  render state =
    let tbl = nameSpaceTable state
        tbl' = M.filterWithKey (\k _ -> M.notMember k builtIns) tbl in
    line $ mconcat ["Names: ", render tbl']

instance Render [M.Map Name Type] where
  render mps = line $ "[" <> (T.intercalate ", " $ map render mps) <> "]"

instance Render NameSpace where
  render = T.intercalate "/" . reverse

-- | Pulled this one out since the behavior is almost the same, but not
-- identical, between `typeOf` and `litTypeOf`
typeOfArrayLiteral typeOfFunc arr = case arr of
  ArrayLiteral [] -> arrayOf <$> unusedTypeVar
  ArrayLiteral (e:exprs) -> do
    (t:types) <- mapM typeOfFunc (e:exprs)
    -- later, we'll relax this to allow disparate types unified by interface
    case all (==t) types of
      True -> return $ arrayOf t
      False -> throwError1 "Multiple types found in the same array"

-- | Later, we'll change this to be a "compatible-with" function to
-- compare two types
(~=) = (==)

-- | Similarly, inferring the type of an application is slightly different
-- when looking for a literal type, so we provide which typeOf
-- function to apply as an argument to typeOfApply
typeOfApply typeOf func arg = do
  argT <- typeOf arg
  retT <- unusedTypeVar
  funcT <- typeOf func
  unifyAdd funcT (argT ==> retT) `catchError` uniError
  refine retT
  where uniError = addError' ["When attempting to apply `", render func
                             , " to argument ", render arg]

unifyAdd t1 t2 = unify t1 t2 >>= mapM_ (uncurry addTypeAlias) . M.toList

addTypeAlias name typ =
  modify $ \s -> s { aliases = M.insert name typ (aliases s)}

instance Typable Expr where
  typeOf expr = go `catchError` err where
    condError = addError "Condition should be a Bool"
    branchError = addError "Parallel branches must resolve to the same type"
    scopeError name = throwErrorC ["'", name, "' is already defined in scope"]
    go = case expr of
      Number _ -> return numT
      String _ -> return strT
      TypeDef name typ -> addTypeAlias name typ >> return unitT
      Block blk -> pushNameSpace "%b" *> typeOf blk <* popNameSpace
      Var name -> lookupAndInstantiate name
      Constructor name -> lookupAndInstantiate name
      Array arr@(ArrayLiteral _) -> typeOfArrayLiteral typeOf arr
      Tuple vals -> tTuple <$> mapM typeOf vals
      Mut expr -> TMut <$> typeOf expr
      Lambda arg expr -> do
        argT <- pushNameSpace "%l" *> litTypeOf arg
        returnT <- typeOf expr
        (refine (argT ==> returnT) >>= generalize) <* popNameSpace
      Dot a b -> typeOf (Apply b a)
      Apply a b -> typeOfApply typeOf a b
      If' cond res -> do
        condT <- typeOf cond
        unifyAdd condT boolT `catchError` condError
        maybeT <$> typeOf res
      If cond tBranch fBranch -> do
        -- Type the condition, make sure it's a Bool
        condType <- typeOf cond
        condType `unifyAdd` boolT `catchError` condError
        -- Type the true and false branches, make sure they're the same type
        trueType <- typeOf tBranch
        falseType <- typeOf fBranch
        trueType `unifyAdd` falseType `catchError` branchError
        -- Return the true branch's refined type (same as false's)
        refine trueType
      Define name expr ->
        lookup1 name >>= \case
          -- If it's not defined in this scope, we proceed forward.
          Nothing -> do
            -- Initialize this name as an unknown type variable, for in case
            -- this is a recursive definition
            var <- unusedTypeVar
            record name var
            result <- pushNameSpace name *> typeOf expr <* popNameSpace
            var' <- refine var
            -- Make sure the types unify
            unify var' result `catchError` definitionError name
            refine result >>= record name
          Just typ -> scopeError name
      Extend name expr@(Lambda arg body) ->
        lookup name >>= \case
          -- If it's not defined in this scope, we proceed forward.
          Nothing -> notInScopeError name
          Just f@(TFunction from to) -> extend name f expr
          Just mf@(TMultiFunc tset) -> extend name mf expr
          Just _ -> notAFunctionError
      Extend _ _ -> notAFunctionError
      Assign expr expr' -> do
        exprT <- typeOf expr
        -- check mutability of exprT here?
        exprT' <- typeOf expr'
        exprT `unify` exprT'
        refine exprT
      While cond block -> do
        pushNameSpace "%while"
        cType <- typeOf cond
        cType `unify` boolT `catchError` condError
        maybeT <$> typeOf block <* popNameSpace
      Return expr -> typeOf expr
      _ -> error $ T.unpack $ "we can't handle expression `" <> render expr <> "'"
    err = addError' ["When typing the expression `", render expr, "'"]
    definitionError name =
      addError' ["When attempting to unify the perceived type of '", name, "' "
                , "(as determined from its declared arguments) with how it is "
                , "used recursively in its definition."]
    itemError name =
      addError' ["When unifying declared type of iterating variable '", name
                , "' with what its container contains"]
    notAFunctionError =
      throwErrorC [ "Only functions' definitions can be extended, and "
                  , "only with other functions."]
    notInScopeError name =
      throwErrorC [ "Attempted to extend the definition of '", name, "', ",
                    "but that name is not in scope."]
    extend name origType expr = do
      -- the expr must be a Lambda with declared argument type.
      argT <- case expr of
        Lambda arg body -> litTypeOf arg
        _ -> notAFunctionError
      -- We don't know what the return type is; initialize it as unknown.
      retT <- unusedTypeVar
      -- Add in this new definition to the current
      record name (origType <> (argT ==> retT))
      result <- pushNameSpace name *> typeOf expr <* popNameSpace
      -- TODO: Make sure the types unify.
      typ <- refine result
      record name (origType <> typ)
      case typ of
        TFunction _ _ -> record name (origType <> typ)
        TMultiFunc  _ -> record name (origType <> typ)
        _ -> notAFunctionError

instance Typable (Expr, Block) where
  typeOf (arg, block) = do
    argType <- litTypeOf arg
    TFunction argType <$> typeOf block

-- | @litType@ is for expressions from which a "literal type" can be
-- determined; that is, context-free. For example @[1,2,3]@ is literally
-- @[Number]@ regardless of context, and @True@ is always @Bool@. Arguments
-- to a function must have a literal type.
litTypeOf :: Expr -> Typing Type
litTypeOf expr = case expr of
  Typed (Var name) typ -> do
    log' ["instantiating typed variable ", name, " as ", render typ]
    instantiated <- instantiate typ
    record name instantiated
  Number _ -> return numT
  String _ -> return strT
  Tuple exprs -> tTuple <$> mapM litTypeOf exprs
  Array arr@(ArrayLiteral _) -> typeOfArrayLiteral litTypeOf arr
  Constructor name -> do
    log' ["instantiating constructor ", name]
    lookupAndInstantiate name
  Apply a b -> typeOfApply litTypeOf a b
  _ -> throwErrorC [ "`", render expr, "' does not have a literal type. "
                   , "Please provide the type of the expression."]

instance Typable Block where
  -- Returns the type of the last statement. Operates in a new context.
  typeOf block = do
    env <- get
    log' ["typing the block `", render block, "' with environment `", render env, "'"]
    go `catchError` err
    where
      err = addError' ["When typing the block `", render block, "'"]
      go = case block of
        [] -> return unitT -- shouldn't encounter this, but...
        (Break expr):_ -> typeOf expr
        [expr] -> typeOf expr
        (Return expr):_ -> typeOf expr
        expr:block -> typeOf expr *> typeOf block
      isReturn (Return _) = True
      isReturn _ = False

getAliases = get <!> aliases

pushNameSpace :: Name -> Typing ()
pushNameSpace name = modify $ \s -> s { nameSpace = name : nameSpace s }
popNameSpace = modify $ \s -> s { nameSpace = tail $ nameSpace s }
popNameSpaceWith name typ = popNameSpace *> record name typ

record :: Name -> Type -> Typing Type
record name typ = do
  nsName <- fullName name
  nsTable <- M.insert nsName typ <$> nameSpaceTable <$> get
  modify $ \s -> s { nameSpaceTable = nsTable }
  return typ

unusedTypeVar :: Typing Type
unusedTypeVar = do
  -- get the current state
  var <- get <!> freshName
  -- increment the freshName
  modify $ \s -> s { freshName = T.pack $ next var }
  -- wrap it in a type variable and return it
  return $ TPolyVar var
  where
    next n = let name = T.unpack n
                 (c:cs) = reverse name in
      if c < '9' then reverse $ succ c : cs
      else if (head name) < 'z' then (succ $ head name) : "0"
      else map (\_ -> 'a') name <> "0"

getNameSpace :: Typing [Name]
getNameSpace = get <!> nameSpace
getTable = get <!> nameSpaceTable

lookup, lookup1 :: Name -> Typing (Maybe Type)
-- | local lookup, searches head of table list
lookup1 name = do
  fullName <- getNameSpace <!> (name :) <!> render
  table <- getTable
  return $ M.lookup name table
-- | recursive lookup, searches up through all symbol tables
lookup name = do
  ns <- get <!> nameSpace
  tbl <- get <!> nameSpaceTable
  loop tbl ns
  where loop tbl [] = return $ M.lookup name tbl
        loop tbl (n:ns) = case M.lookup (render $ name:n:ns) tbl of
          Just typ -> return (Just typ)
          Nothing -> loop tbl ns

-- | looks up all of the functions in scope under the given name
lookupFuncs :: Name -> Typing [Type]
lookupFuncs funcName = do
  tbl <- getTable
  names <- getNameSpace
  go tbl [] names
  where
    go :: TypeTable -> [Type] -> [Name] -> Typing [Type]
    go tbl acc nspace = case nspace of
      [] -> fmap (<>acc) $ check tbl funcName
      nspace -> do
        types <- check tbl (render $ funcName : nspace)
        go tbl (types <> acc) (tail nspace)

    check :: TypeTable -> Name -> Typing [Type]
    check tbl name = case M.lookup name tbl of
      Just (t@(TFunction from to)) -> return [t]
      Just (t@(TRigidVar name))    -> return [t]
      Just (t@(TPolyVar name))     -> return [t]
      Just t -> throwErrorC ["Identifier `", name, "' maps to non-"
                            , "function type `", render t, "'"]
      Nothing -> return mempty

lookupAndInstantiate :: Name -> Typing Type
lookupAndInstantiate name = lookup name >>= \case
  Just typ -> instantiate typ
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

-- | adds to the type aliases map any substitutions needed to make
-- its two arguments equivalent. Throws an error if such a substitution
-- is impossible.
-- TODO: throw an error if there's a cycle
unify :: Type -> Type -> Typing (M.Map Name Type)
unify type1 type2 = snd <$> runStateT (go (type1, type2)) mempty where
  go :: (Type, Type) -> StateT (M.Map Name Type) Typing ()
  go types = do
    let (type1, type2) = types
    lift $ log' ["Unifying `", render type1, "' with `", render type2, "'"]
    case types of
      (a, b) | a == b  -> return ()
      (TMut mtyp, typ) -> go (mtyp, typ)
      (typ, TMut mtyp) -> go (mtyp, typ)
      (TPolyVar name, typ) -> alias name typ
      (typ, TPolyVar name) -> alias name typ
      (TConst name, TConst name') | name == name' -> return ()
      (TTuple ts, TTuple ts') | length ts == length ts' -> mapM_ go $ zip ts ts'
      (TApply a b, TApply a' b') -> go (a, a') >> go (b, b')
      (TFunction a b, TFunction a' b') -> do
        lift $ log' ["a, a', b, b' are, ", T.intercalate ", " $ map render [a, a', b, b']]
        go (a, a') `catchError'` argError
        lift $ log' ["bloops!"]
        go (b, b') `catchError'` returnError
        lift $ log' ["poops!"]
      (TMultiFunc set, TFunction from to) -> do
        lift $ log' ["poooooooooooooooooooooooooooooooooooooop"]
        -- Go through the multifunction's argument types. If we can
        -- unify @from@ with an argument type, record its subs and
        -- pair it with the @to@ type that argument type maps to.
        -- Valid choices get wrapped in a @Just@, otherwise @Nothing@.
        subsTos <- forM (M.toList set) $ \(from', to') -> do
          lift $ log' ["trying to unify `", render from', "' with `", render from, "'"]
          subs <- lift $ unify from' from `catchError` \_ -> do log' ["blaaaaaaaaaaaat"]
                                                                return mempty
          (lift (unify from' from) >>= \s -> return (Just (s, to')))
                  `catchError` \_ -> return Nothing
        -- Filter out the @Nothing@ values and sort by smallest subs.
        let valids = catMaybes subsTos ! sortWith (fst ~> M.size)
        lift $ log' $ map T.pack ["valids: ", show valids]
        when (length valids == 0) $ noArgMatchErr from set
        tryAll to valids `catchError` tryingErr valids
      (type1, type2) -> incompatErr
  tryAll :: Type -> [(M.Map Name Type, Type)] -> StateT (M.Map Name Type) Typing ()
  tryAll to valids = case valids of
    -- If there aren't any valids, it's incompatible.
    [] -> lift noMatchErr
    -- If there's exactly one, try to unify @to@ with it after adding the subs.
    [(subs, to')] -> addSubs subs >> lift (unify to' to) >>= addSubs
    -- If the first two are of equal size, then it's ambiguous, an error.
    (s1, _):(s2, _):_ | M.size s1 == M.size s2 -> lift $ ambiguousErr s1 s2
    -- Otherwise, try to unify, and if it fails, move on to the next.
    (subs, to'):rest -> do
      savedSubs <- get
      addSubs subs
      (lift (unify to' to) >>= addSubs) `catchError` \_ ->
        put savedSubs >> tryAll to rest

  alias name typ = M.lookup name <$> get >>= \case
    Nothing -> modify $ M.insert name typ
    Just (TPolyVar name') -> alias name' typ
    Just typ' | typ == typ' -> return ()
              | otherwise -> case typ of
                TPolyVar name' -> alias name' (TRigidVar name)
                _ -> throwError1 "Occurs check"

  addSubs = modify . M.union
  argError = addError' ["When attempting to unify the argument types of `"
                       , render type1, "' and `", render type2, "'"]
  returnError = addError' ["When attempting to unify the return types of `"
                          , render type1, "' and `", render type2, "'"]
  catchError' = catchError
  incompatErr =
      throwErrorC ["Incompatible types: `", render type1, "' and `", render type2, "'"]
  ambiguousErr t1 t2 = throwErrorC [
      "Ambiguous application of `", render type1, "' to `", render type2, "'. "
    , "Multiple unification choices are equally valid: could be `", render t1
    , "', or `", render t2, "'"
    ]
  tryingErr valids = addError' ["After finding potential matches ", r valids]
  r valids = T.intercalate ", " $ map (fst ~> render) valids
  noMatchErr = throwErrorC ["No types exist in multifunction's set which match ",
                            render type2]
  noArgMatchErr argT set =
    throwErrorC ["No types in the set `", render (M.keys set), "' match the "
                , "provided argument type `", render argT, "'"]

-- | takes a type and replaces any type variables in the type with unused
-- variables. Note: in Hindley-Milner, there are two distinct types, Type and
-- Polytype, and instantiate maps between them. Tentatively, we don't need this
-- distinction.
instantiate :: Type -> Typing Type
instantiate typ = fst <$> runStateT (inst typ) mempty where
  inst :: Type -> StateT (M.Map Name Type) Typing Type
  inst typ = case typ of
    TRigidVar name -> return typ
    TConst name -> return typ
    TPolyVar name -> do
      M.lookup name <$> get >>= \case
        -- if we haven't yet seen this variable, create a new one
        Nothing -> do typ' <- lift unusedTypeVar
                      modify $ M.insert name typ'
                      return typ'
        -- otherwise, return what we already created
        Just typ' -> return typ'
    TApply a b -> TApply <$$ inst a <*> inst b
    TFunction a b -> TFunction <$$ inst a <*> inst b
    TTuple ts -> TTuple <$> mapM inst ts
    TMut typ -> TMut <$> inst typ
    TMultiFunc tset -> do
      list' <- forM (M.toList tset) $ \(f, t) -> (,) <$$ inst f <*> inst t
      return $ TMultiFunc (M.fromList list')

-- | the opposite of instantiate; it "polymorphizes" the rigid type variables
-- so that they can be polymorphic in future uses.
generalize :: Type -> Typing Type
generalize typ = case typ of
  TRigidVar name -> return $ TPolyVar name
  TPolyVar name -> return typ
  TConst name -> return typ
  TTuple ts -> TTuple <$> mapM generalize ts
  TApply a b -> TApply <$$ generalize a <*> generalize b
  TFunction a b -> TFunction <$$ generalize a <*> generalize b

-- | follows the type aliases and returns the fully qualified type (as
-- qualified as possible)
refine :: Type -> Typing Type
refine typ = fst <$> runStateT (look typ) mempty where
  --look :: S.Set Name -> Type -> Typing Type
  look typ = case typ of
    TConst _ -> return typ
    TRigidVar _ -> return typ
    TMut typ' -> TMut <$> look typ'
    TPolyVar name -> do
      seenNames <- get
      if name `S.member` seenNames then throwError1 "Cycle in type aliases"
      else do
        M.lookup name <$> (lift getAliases) >>= \case
          Nothing -> return typ
          Just typ' -> modify (S.insert name) >> look typ'
    TFunction a b -> TFunction <$$ look a <*> look b
    TApply a b -> TApply <$$ look a <*> look b
    TTuple ts -> TTuple <$> mapM look ts

defaultTypingState = TypingState { aliases = mempty
                                 , nameSpace = []
                                 , nameSpaceTable = builtIns
                                 , freshName = "a0"}

fullName :: Name -> Typing T.Text
fullName name = get <!> nameSpace <!> (name:) <!> render

builtIns = M.fromList [ ("+", join [nnn, sss, css, scs, vvv, avv, vav])
                      , ("-", nnn), ("/", nnn)
                      , ("*", nnn `or` sns)
                      , ("%", nnn), (">", nnb), ("<", nnb), (">=", nnb)
                      , ("<=", nnb), ("==", nnb `or` ssb), ("!=", nnb `or` ssb)
                      , ("<|", tup ab a b), ("|>", tup a ab b)
                      , ("~>", tup ab bc ac), ("<~", tup bc ab ac)
                      , ("!_", boolT ==> boolT), ("_!", numT ==> numT)
                      , ("print", a ==> unitT)
                      , ("Just", a ==> maybeT a)
                      , ("Nothing", maybeT a)
                      , ("@call", nnn `or` sss `or` vna)
                      , ("True", tConst "Bool")
                      , ("False", tConst "Bool") ]
  where tup a b c = tTuple [a, b] ==> c
        tup' a b c = (tTuple [a, b], c)
        nnn = tup numT numT numT
        sss = tup strT strT strT
        scs = tup strT charT strT
        css = tup charT strT strT
        sns = tup strT numT strT
        vna = tup (arrayOf a) numT a
        vvv = tup (arrayOf a) (arrayOf a) (arrayOf a)
        vav = tup (arrayOf a) a (arrayOf a)
        avv = tup a (arrayOf a) (arrayOf a)
        nnnOrSss = TMultiFunc $ M.fromList [ tup' strT strT strT
                                           , tup' numT numT numT ]
        nnb = tup numT numT boolT
        ssb = tup strT strT boolT
        nnbOrSsb = TMultiFunc $ M.fromList [ tup' strT strT boolT
                                           , tup' numT numT boolT ]
        [a, b, c] = TPolyVar <$> ["a", "b", "c"]
        (ab, bc, ac) = (a ==> b, b ==> c, a ==> c)
        join ts = foldr1 or ts
        TMultiFunc s `or` TMultiFunc s' = TMultiFunc $ M.union s s'
        TMultiFunc s `or` TFunction from to = TMultiFunc $ M.insert from to s
        TFunction from to `or` TMultiFunc s = TMultiFunc $ M.insert from to s
        TFunction f1 t1 `or` TFunction f2 t2 =
          TMultiFunc $ M.fromList [(f1, t1), (f2, t2)]
        t1 `or` t2 = error $ "Invalid `or`s: " <> show t1 <> ", " <> show t2

-- NOTE: using unsafePerformIO for testing purposes only. This will
-- all be pure code in the end.
runTypingWith :: Typable a => TypingState -> a -> (Either ErrorList Type, TypingState)
runTypingWith state a = unsafePerformIO $ runStateT (runErrorT $ typeOf a) state

runTyping :: Typable a => a -> (Either ErrorList Type, TypingState)
runTyping = runTypingWith defaultTypingState

runUnify :: Type -> Type -> (Either ErrorList (M.Map Name Type), TypingState)
runUnify type1 type2 =
  unsafePerformIO $ runStateT (runErrorT $ unify type1 type2) defaultTypingState

typeItIO :: String -> IO ()
typeItIO input = case grab input of
  Left err -> putStrLn $ "Parse error:\n" <> show err
  Right block -> case runTyping block of
    (Left err, _) -> error $ T.unpack $ render err
    (Right block, state) -> putStrLn $ T.unpack (render block <> "\n" <> render state)

typeIt :: String -> Either ErrorList Type
typeIt input = case grab input of
  Left err -> Left $ TE ["Parse error:\n" <> (T.pack $ show err)]
  Right block -> fst $ runTyping block

unifyIt :: (String, String) -> Either ErrorList (M.Map Name Type)
unifyIt (input1, input2) = do
  type1 <- grabT input1
  type2 <- grabT input2
  fst $ runUnify type1 type2

log :: T.Text -> Typing ()
log s = if hideLogs then return () else lift2 $ putStrLn $ T.unpack s
log' = mconcat ~> log

lift2 = lift . lift


hideLogs = True
