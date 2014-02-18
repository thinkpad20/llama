{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module TypeChecker where

import Prelude hiding (lookup, log)
import System.IO.Unsafe
import Control.Monad.Error.Class
import Common
import AST
import Parser (grab)
import qualified Data.Map as M
import qualified Data.Set as S

class Typable a where
  typeOf :: a -> Typing Type

newtype TypingError = TE [String]
data TypingState = TypingState { table ::[M.Map Name Type]
                               , aliases :: M.Map Name Type
                               , freshName :: Name }
type Typing = ErrorT TypingError (StateT TypingState IO)

instance Error TypingError where
  strMsg = TE . pure

instance Show TypingError where
  show (TE msgs) = msgs ! concatMap ("\n  "++) ! (++ "\n")

instance Show TypingState where
  show state = concat ["Table: ", show $ init $ table state, ", "
                      , "Aliases: ", show $ aliases state]

instance Show (M.Map Name Type) where
  show mp = "{" ++ intercalate ", " pairs ++ "}" where
    pairs = mp ! M.toList ! map (\(n, t) -> n ++ ": " ++ show t)

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

-- | Similarly, inferring the type of an application is slightly different
-- when looking for a literal type
typeOfApply typeOf func arg = do
  log' ["determining the type of ", show func, " applied to ", show arg]
  funcType <- typeOf func
  argType <- typeOf arg
  log' ["determined funcType to be `", show funcType
       , "' and argType to be `", show argType, "'"]
  case funcType of
    TFunction argType' returnType -> do
      unify argType argType' `catchError` uniError
      refine returnType
    -- here we can check if its type is Functionable?
    _ -> throwErrorC ["Expression `", show func, "' is not a function, "
                     , "but of type `", show funcType, "'"]
  where uniError = addError' ["When attempting to apply `", show func
                             , "' to argument `", show arg]

addTypeAlias name typ =
  modify $ \s -> s { aliases = M.insert name typ (aliases s)}

instance Typable Expr where
  typeOf expr = go `catchError` err where
    go = case expr of
      Number _ -> return numT
      String _ -> return strT
      Var name -> do
        log' ["instantiating expr var '", name, "'"]
        lookupAndInstantiate name
      Constructor name -> do
        log' ["instantiating expr constructor '", name, "'"]
        lookupAndInstantiate name
      Array arr@(ArrayLiteral _) -> typeOfArrayLiteral typeOf arr
      Tuple vals -> TTuple <$> mapM typeOf vals
      Lambda argsAndBodies -> do
        -- get all of the arguments and bodies
        t:types <- mapM typeOf argsAndBodies
        -- make sure they're all the same
        mapM_ (unify t) types `catchError` altError
        return t
      Apply a b -> typeOfApply typeOf a b
      _ -> error $ "we can't handle expression `" ++ show expr ++ "'"
    err = addError' ["When typing the expression `", show expr, "'"]
    altError = addError "All alternatives in a lambda must have the same type"

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
    log' ["instantiating typed variable ", name, " as ", show typ]
    instantiate typ >>= setType name
  Number _ -> return numT
  String _ -> return strT
  Tuple exprs -> TTuple <$> mapM litTypeOf exprs
  Array arr@(ArrayLiteral _) -> typeOfArrayLiteral litTypeOf arr
  Constructor name -> do
    log' ["instantiating constructor ", name]
    lookupAndInstantiate name
  Apply a b -> typeOfApply litTypeOf a b
  _ -> throwErrorC [ "`", show expr, "' does not have a literal type. "
                   , "Please provide the type of the expression."]

instance Typable Block where
  -- Returns the type of the last statement. Operates in a new context.
  typeOf block = push *> go `catchError` err <* pop
    where
      err = addError' ["When typing the block `", show block, "'"]
      go = case block of
        [] -> return $ TTuple [] -- shouldn't encounter this, but...
        Break:_ -> return $ TTuple []
        [stmt] -> typeOf stmt
        (Return expr):_ -> typeOf expr
        (If' expr blk1):blk2 | any isReturn blk1 -> do
          blk1Type <- typeOf blk1
          blk2Type <- typeOf blk2
          case blk1Type == blk2Type of
            True -> return blk1Type
            False -> throwErrorC ["Two branches have different types: `"
                                 , show blk1Type, "' and `", show blk1Type, "'"]
        stmt:block -> typeOf stmt *> typeOf block
      isReturn (Return _) = True
      isReturn _ = False

instance Typable Statement where
  typeOf statement = go `catchError` err where
    err = case statement of
      Expr expr -> throwError
      _ -> addError' ["When typing the statement `", show statement, "'"]
    condError = addError "Condition should be a Bool"
    branchError = addError "Parallel branches must resolve to the same type"
    scopeError name = throwErrorC ["'", name, "' is already defined in scope"]
    go = case statement of
      Expr expr -> typeOf expr
      If cond tBranch fBranch -> do
        cType <- typeOf cond
        cType `unify` boolT `catchError` condError
        tType <- typeOf tBranch
        fType <- typeOf fBranch
        tType `unify` fType `catchError` branchError
        refine tType
      Define (Var name) block ->
        lookup1 name >>= \case
          Nothing -> typeOf block >>= setType name
          Just typ -> scopeError name
      Define (Typed (Var name) typ) block ->
        lookup1 name >>= \case
          Just typ -> scopeError name
          Nothing -> do
            typ' <- typeOf block
            typ `unify` typ'
            refine typ
      Assign expr block -> do
        exprT <- typeOf expr
        -- check mutability of exprT here?
        blockT <- typeOf block
        exprT `unify` blockT
        refine exprT
      While cond block -> do
        cType <- typeOf cond
        cType `unify` boolT `catchError` condError
        typeOf block
      For (Var name) container block -> do
        -- need to make sure container contains things...
        contT <- typeOf container
        -- we probably want to do this via traits instead of this, but eh...
        case contT of
          TApply typ typ' -> setType name typ' >> typeOf block
          typ -> throwErrorC ["Non-container type `", show typ
                             , "' used in a for loop"]

getTable = get <!> table
getAliases = get <!> aliases

-- | self-explanatory
push, pop :: Typing ()
push = modify $ \s -> s {table = mempty : table s}
pop = modify $ \s -> s {table = tail $ table s}

setType :: Name -> Type -> Typing Type
setType name typ = do
  tbl:tbls <- getTable
  modify $ \s -> s {table = M.insert name typ tbl : tbls}
  return typ

unusedTypeVar :: Typing Type
unusedTypeVar = do
  -- get the current state
  var <- get <!> freshName
  -- increment the freshName
  modify $ \s -> s { freshName = next var }
  -- wrap it in a type variable and return it
  return $ TVar Polymorphic var
  where
    next name = let (c:cs) = reverse name in
      if c < '9' then reverse $ succ c : cs
      else if (head name) < 'z' then (succ $ head name) : "0"
      else map (\_ -> 'a') name ++ "0"

log :: String -> Typing ()
log = lift . lift . putStrLn
log' = concat ~> log

lookup, lookup1 :: Name -> Typing (Maybe Type)
-- | local lookup, searches head of table list
lookup1 name = getTable >>= head ~> M.lookup name ~> return
-- | recursive lookup, searches up through all symbol tables
lookup name = getTable >>= loop
  where loop [] = return Nothing
        loop (tbl:tbls) = case M.lookup name tbl of
          Just typ -> return (Just typ)
          Nothing -> loop tbls

lookupAndInstantiate :: Name -> Typing Type
lookupAndInstantiate name = lookup name >>= \case
  Just typ -> instantiate typ
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

throwErrorC = throwError1 . concat
throwError1 = throwError . TE . pure
addError msg (TE msgs) = throwError $ TE $ msg : msgs
addError' = addError . concat

builtIns = M.fromList [ ("+", nnn), ("-", nnn), ("*", nnn), ("/", nnn)
                      , ("%", nnn), (">", nnb), ("<", nnb), (">=", nnb)
                      , ("<=", nnb), ("==", nnb), ("!=", nnb)
                      , ("<|", ab ==> a ==> b), ("|>", a ==> ab ==> b)
                      , ("~>", ab ==> bc ==> ac), ("<~", bc ==> ab ==> ac)
                      , ("print", a ==> unitT)
                      , ("Just", a ==> TApply (TConst "Maybe") a)
                      , ("Nothing", TApply (TConst "Maybe") a) ]
  where nnn = numT ==> numT ==> numT
        nnb = numT ==> numT ==> boolT
        [a, b, c] = TVar Polymorphic <$> ["a", "b", "c"]
        (ab, bc, ac) = (a ==> b, b ==> c, a ==> c)
defaultState = TypingState { table = [builtIns]
                           , aliases = mempty
                           , freshName = "a0"}

-- NOTE: using unsafePerformIO for testing purposes only. This will
-- all be pure code in the end.
runTyping :: Typable a => a -> (Either TypingError Type, TypingState)
runTyping a = unsafePerformIO $ runStateT (runErrorT $ typeOf a) defaultState

typeIt :: String -> (Either TypingError Type, TypingState)
typeIt input = case grab input of
  Left err -> error $ show err
  Right block -> runTyping block

-- | adds to the type aliases map any substitutions needed to make
-- its two arguments equivalent. Throws an error if such a substitution
-- is impossible (todo: throw an error if there's a cycle)
unify type1 type2 | type1 == type2 = return ()
                  | otherwise = case (type1, type2) of
  (TVar Polymorphic name, typ) -> addTypeAlias name typ
  (typ, TVar Polymorphic name) -> addTypeAlias name typ
  (TTuple ts, TTuple ts') -> mapM_ (uncurry unify) $ zip ts ts'
  (TApply a b, TApply a' b') -> do
    unify a a' `catchError` operandsError " first "
    unify b b' `catchError` operandsError " second"
  _ -> throwErrorC ["Incompatible types: `", show type1, "' and `", show type2, "'"]
  where
    operandsError which = addError' ["When unifying the", which, "operands of `"
                                    , show type1, "' and `", show type2]

-- | takes a type and replaces any type variables in the type with unused
-- variables.
instantiate typ = fst <$> runStateT (inst typ) mempty where
  inst :: Type -> StateT (M.Map Name Type) Typing Type
  inst typ = do
    lift $ log' ["instantiating type `", show typ, "'"]
    mp <- get
    lift $ log' ["Map contains ", show mp]
    result <-
      case typ of
        TVar Rigid name -> return typ
        TVar Polymorphic name -> do
          M.lookup name <$> get >>= \case
            -- if we haven't yet seen this variable, create a new one
            Nothing -> do typ' <- lift unusedTypeVar
                          modify $ M.insert name typ'
                          return typ'
            -- otherwise, return what we already created
            Just typ' -> return typ'
        TConst _ -> return typ
        TTuple types -> TTuple <$> mapM inst types
        TApply a b -> TApply <$$ inst a <*> inst b
        TFunction a b -> TFunction <$$ inst a <*> inst b
    lift $ log' ["found result `", show result, "'"]
    return result

-- | follows the type aliases and returns the fully qualified type (as
-- qualified as possible)
refine typ = look mempty typ where
  look :: S.Set Name -> Type -> Typing Type
  look seenNames typ = case typ of
    TConst _ -> return typ
    TVar Rigid _ -> return typ
    TTuple types -> TTuple <$> mapM (look seenNames) types
    TVar Polymorphic name
      | name `S.member` seenNames -> throwError1 "Cycle in type aliases"
      | otherwise -> do
      M.lookup name <$> getAliases >>= \case
        Nothing -> return typ
        Just typ' -> look (S.insert name seenNames) typ'
    TApply a b -> TApply <$$ look seenNames a <*> look seenNames b
    TFunction a b -> TFunction <$$ look seenNames a <*> look seenNames b
