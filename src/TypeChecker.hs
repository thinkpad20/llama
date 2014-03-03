{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeChecker ( Typable(..), Typing, TypeTable, TypingState(..)
                   , runTyping, runTypingWith, defaultTypingState) where

import Prelude hiding (lookup, log)
import System.IO.Unsafe
import Control.Monad.Error.Class
import Common
import AST
import Parser (grab)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

class Typable a where
  typeOf :: a -> Typing Type

data TypeRecord = Type Type | TypeSet TypeSet deriving (Show)
type TypeSet = S.Set Type
type TypeTable = M.Map Name TypeRecord
type NameSpace = [Name]
data TypingState = TypingState { aliases :: M.Map Name Type
                               , nameSpace :: [Name]
                               , nameSpaceTable :: TypeTable
                               , freshName :: Name } deriving (Show)
type Typing = ErrorT ErrorList (StateT TypingState IO)

instance Render TypeRecord where
  render (Type typ) = render typ
  render (TypeSet ts) = "{" <> (T.intercalate ", " $ map render $ S.elems ts) <> "}"

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
  log' ["determining the type of ", render func, " applied to ", render arg]
  argT <- typeOf arg
  case func of
    Var name -> do
      tList <- lookupFuncs name
      log' ["function is a name, '", name, "' lookup returned `", render tList, "'"]
      when (length tList == 0) $ throwErrorC ["`", name, "' is not defined in scope"]
      getFirstCompatible argT name tList
    _ -> do
      log' ["not a name, but `", T.pack $ show func, "'"]
      typeOf func >>= \case
        TFunction fromT returnT -> do
          unify argT fromT `catchError` uniError argT fromT
          refine returnT
        -- here we can check if we can use the type "as a function"
        funcT -> typeOf (Apply (Var "@call") (Tuple [func, arg]))
          `catchError` \_ ->
            throwErrorC ["No way to call the expression `", render func
                        , "' as a function with argument `", render argT
                        , "'. Define `@call` if desired."]
  where uniError typ typ' = addError' ["`", render func, "' takes `"
                                      , render typ', "' as its argument, but `"
                                      , render arg, "' has the type `"
                                      , render typ, "'"]

getFirstCompatible argT funcName tList = go tList where
  go :: [Type] -> Typing Type
  go list = case list of
    (TFunction fromT toT):rest -> do
      log' ["Trying to unify `", render argT, "' with `", render fromT, "'"]
      (unify argT fromT >> refine toT) `catchError` \_ -> go rest
    funcT@(TPolyVar _):_ -> do
      returnT <- unusedTypeVar
      unify funcT (TFunction argT returnT)
      refine returnT
    typ:rest -> throwErrorC ["Identifier `", funcName, "' has non-function "
                            , "type `", render typ, "'"]
    [] -> throwErrorC ["Function `", funcName, "' can accept types "
                      , render tList, " but none of these match the type of "
                      , "its argument `", render argT, "'"]

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
      Var name -> do
        --env <- get
        --log' ["instantiating expr var '", name, "' in environment ", render env]
        lookupAndInstantiate name >>= check
      Constructor name -> do
        log' ["instantiating expr constructor '", name, "'"]
        lookupAndInstantiate name >>= check
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
        unify condT boolT `catchError` condError
        maybeT <$> typeOf res
      If cond tBranch fBranch -> do
        cType <- typeOf cond
        cType `unify` boolT `catchError` condError
        tType <- typeOf tBranch
        fType <- typeOf fBranch
        tType `unify` fType `catchError` branchError
        refine tType
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
      For expr container block -> do
        pushNameSpace "%for"
        -- need to make sure container contains things...
        contT <- typeOf container
        -- we probably want to do this via traits instead of this, but eh...
        case (expr, contT) of
          (Var name, TApply contT itemT) -> do
            record name itemT
            result <- typeOf block
            popNameSpaceWith name result
          (Typed (Var name) typ, TApply contT itemT) -> do
            unify typ itemT `catchError` itemError name
            refine itemT >>= record name
            result <- typeOf block
            popNameSpaceWith name result
          (_, typ) -> throwErrorC ["Non-container type `", render typ
                                  , "' used in a for loop"]
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

instance Typable (Expr, Block) where
  typeOf (arg, block) = do
    argType <- litTypeOf arg
    TFunction argType <$> typeOf block

check :: TypeRecord -> Typing Type
check (Type typ) = return typ
check (TypeSet ts) = throwErrorC [ "Error: computed an ambiguous type: it "
                                 , "could be any of ", render ts, ". Each "
                                 , "expression must have a single type."]

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
    lookupAndInstantiate name >>= check
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
  nsTable <- M.insert nsName (Type typ) <$> nameSpaceTable <$> get
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

lookup, lookup1 :: Name -> Typing (Maybe TypeRecord)
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
      Just (Type t@(TFunction from to)) -> return [t]
      Just (Type t@(TRigidVar name))    -> return [t]
      Just (Type t@(TPolyVar name))     -> return [t]
      Just (Type t) -> throwErrorC ["Identifier `", name, "' maps to non-"
                                   , "function type `", render t, "'"]
      Just (TypeSet ts) -> return $ S.elems ts
      Nothing -> return mempty

lookupAndInstantiate :: Name -> Typing TypeRecord
lookupAndInstantiate name = lookup name >>= \case
  Just (Type typ) -> Type <$> instantiate typ
  Just (TypeSet ts) -> (TypeSet . S.fromList) <$> mapM instantiate (S.toList ts)
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

-- | adds to the type aliases map any substitutions needed to make
-- its two arguments equivalent. Throws an error if such a substitution
-- is impossible (todo: throw an error if there's a cycle)
unify :: Type -> Type -> Typing Int
unify type1 type2 = snd <$> runStateT (go type1 type2) 0 where
  go :: Type -> Type -> StateT Int Typing ()
  go type1 type2 = do
    lift $ log' ["Unifying `", render type1, "' with `", render type2, "'"]
    case (type1, type2) of
      (a, b) | a == b  -> return ()
      (TMut mtyp, typ) -> go mtyp typ
      (typ, TMut mtyp) -> go mtyp typ
      (TPolyVar name, typ) -> lift (addTypeAlias name typ) <* modify (+1)
      (typ, TPolyVar name) -> lift (addTypeAlias name typ) <* modify (+1)
      (TConst name, TConst name') | name == name' -> return ()
      (TTuple ts, TTuple ts') -> mapM_ (uncurry go) $ zip ts ts'
      (TApply a b, TApply a' b') -> go a a' >> go b b'
      (TFunction a b, TFunction a' b') -> do
        go a a' `catchError'` argError
        go b b' `catchError'` returnError
      _ -> do
        --log' ["Incompatible types: `", show type1, "' and `", show type2, "'"]
        throwErrorC ["Incompatible types: `", render type1, "' and `", render type2, "'"]
  argError = addError' ["When attempting to unify the argument types of `"
                       , render type1, "' and `", render type2, "'"]
  returnError = addError' ["When attempting to unify the return types of `"
                          , render type1, "' and `", render type2, "'"]
  catchError' = catchError

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

builtIns = M.fromList [ ("+", nnnOrSss), ("-", nnn), ("*", nnn), ("/", nnn)
                      , ("%", nnn), (">", nnb), ("<", nnb), (">=", nnb)
                      , ("<=", nnb), ("==", nnbOrSsb), ("!=", nnbOrSsb)
                      , ("<|", Type $ tup ab a b), ("|>", Type $ tup a ab b)
                      , ("~>", Type $ tup ab bc ac), ("<~", Type $ tup bc ab ac)
                      , ("print", Type $ a ==> unitT)
                      , ("Just", Type $ a ==> maybeT a)
                      , ("Nothing", Type $ maybeT a)
                      , ("@call", nnnOrSss), ("True", Type $ tConst "Bool")
                      , ("False", Type $ tConst "Bool") ]
  where tup a b c = tTuple [a, b] ==> c
        nnn = Type $ tup numT numT numT
        sss = Type $ tup strT strT strT
        nnnOrSss = TypeSet $ S.fromList [ tup strT strT strT
                                        , tup numT numT numT ]
        nnb = Type $ tup numT numT boolT
        ssb = Type $ tup strT strT boolT
        nnbOrSsb = TypeSet $ S.fromList [ tup strT strT boolT
                                        , tup numT numT boolT ]
        [a, b, c] = TPolyVar <$> ["a", "b", "c"]
        (ab, bc, ac) = (a ==> b, b ==> c, a ==> c)

-- NOTE: using unsafePerformIO for testing purposes only. This will
-- all be pure code in the end.
runTypingWith :: Typable a => TypingState -> a -> (Either ErrorList Type, TypingState)
runTypingWith state a = unsafePerformIO $ runStateT (runErrorT $ typeOf a) state

runTyping :: Typable a => a -> (Either ErrorList Type, TypingState)
runTyping = runTypingWith defaultTypingState

typeIt :: String -> IO ()
typeIt input = case grab input of
  Left err -> putStrLn $ "Parse error:\n" <> show err
  Right block -> case runTyping block of
    (Left err, _) -> error $ T.unpack $ render err
    (Right block, state) -> putStrLn $ T.unpack (render block <> "\n" <> render state)

log :: T.Text -> Typing ()
log s = lift2 $ putStrLn $ T.unpack s
log' = mconcat ~> log

lift2 = lift . lift
