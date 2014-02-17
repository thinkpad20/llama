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

class Typable a where
  typeOf :: a -> Typing Type

type TypingError = [String]
data TypingState = TypingState { table ::[M.Map Name Type]
                               , aliases :: M.Map Name Type } deriving (Show)
type Typing = ErrorT TypingError (StateT TypingState IO)

instance Error TypingError where
  strMsg = pure

--instance Show TypingState where
--  show state = "Typings: " ++ show (init $ table state)

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

typeOfApply typeOf func arg = do
    funcType <- typeOf func
    argType <- typeOf arg
    case funcType of
      TFunction argType' returnType -> do
        unify argType argType' `catchError` uniError
        return returnType
      -- here we can check if its type is Functionable?
      _ -> throwErrorC ["Expression `", show func, "' is not a function, "
                       , "but of type `", show funcType, "'"]
    where uniError = addError' ["When attempting to apply `", show func
                               , "' to argument `", show arg]

unify type1 type2 | type1 == type2 = return ()
                  | otherwise = case (type1, type2) of
  (TVar name, typ) -> addTypeAlias name typ
  (typ, TVar name) -> addTypeAlias name typ
  (TTuple ts, TTuple ts') -> mapM_ (uncurry unify) $ zip ts ts'
  (TApply a b, TApply a' b') -> do
    unify a a' `catchError` operandsError " first "
    unify b b' `catchError` operandsError " second"
  _ -> throwErrorC ["Incompatible types: `", show type1, "' and `", show type2, "'"]
  where 
    operandsError which = addError' ["When unifying the", which, "operands of `"
                                    , show type1, "' and `", show type2]

addTypeAlias name typ = 
  modify $ \s -> s { aliases = M.insert name typ (aliases s)}

instance Typable Expr where
  typeOf expr = case expr of
    Number _ -> return numT
    String _ -> return strT
    Var name -> lookupAndError name
    Constructor name -> lookupAndError name
    Array arr@(ArrayLiteral _) -> typeOfArrayLiteral typeOf arr
    Tuple vals -> TTuple <$> mapM typeOf vals
    Lambda argsAndBodies -> do
      -- get all of the arguments and bodies
      t:types <- mapM typeOf argsAndBodies
      -- make sure they're all the same
      case all (==t) types of
        True -> return t
        False -> throwError1 "All alternatives in a lambda must have the same type"
    Apply a b -> typeOfApply typeOf a b `catchError` err
    _ -> error $ "we can't handle expression `" ++ show expr ++ "'"
    where err = addError' ["When typing the expression `", show expr, "'"]
instance Show (M.Map Name Type) where
  show mp = "{" ++ intercalate ", " pairs ++ "}" where
    pairs = mp ! M.toList ! map (\(n, t) -> n ++ ": " ++ show t)

instance Typable (Expr, Block) where
  typeOf (arg, block) = do
    argType <- litTypeOf arg
    TFunction argType <$> typeOf block 

-- | @litType@ is for expressions from which a "literal type" can be
-- determined; that is, context-free. For example @[1,2,3]@ is literally 
-- @[Number]@ regardless of context, and @True@ is always @Bool@. Arguments
-- to a function must have a literal type.
{-
ex: (Just 1) => 2
  lookup Just gives a -> Maybe a
    lookup 1 gives Number
      need a unification step here to make it Maybe Number
-}
litTypeOf :: Expr -> Typing Type
litTypeOf expr = case expr of
  (Typed (Var name) typ) -> setType name typ
  Number _ -> return numT
  String _ -> return strT
  Tuple exprs -> TTuple <$> mapM litTypeOf exprs
  Array arr@(ArrayLiteral _) -> typeOfArrayLiteral litTypeOf arr
  Constructor name -> lookupAndError name
  Apply a b -> typeOfApply litTypeOf a b

instance Typable Block where
  -- Returns the type of the last statement. Operates in a new context.
  typeOf block = push *> go <* pop
    where 
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
  typeOf statement = case statement of
    Expr expr -> typeOf expr
    If cond tBranch fBranch -> do
      cType <- typeOf cond
      tType <- typeOf tBranch
      fType <- typeOf fBranch
      case (cType == boolT, tType == fType) of
        (True, True) -> return tType
        -- later we might want to add a "boolable" class
        (False, _) -> throwErrorC [ "Condition type is `", show cType
                                  , "', but it must be a Bool"]
        (_, False) -> throwErrorC [ "True branch in if statement "
                                  , "has type `", show tType, "' is not"
                                  , " the same type as False branch"
                                  , " of type `", show fType, "'"]
    Define (Var name) block -> 
      lookup1 name >>= \case
        Nothing -> typeOf block >>= setType name
        Just typ -> throwErrorC ["'", name, "' is already defined in scope"]
    Define (Typed (Var name) typ) block ->
      lookup1 name >>= \case
        Just typ -> throwErrorC ["'", name, "' is already defined in scope"]
        Nothing -> do
          typ' <- typeOf block
          case typ == typ' of
            True -> setType name typ >> return typ
            False -> throwErrorC ["Asserted type of '", name
                                 , "' to be `", show typ, "', but "
                                 , "inferred as type `", show typ', "'"]
    Assign expr block -> do
      exprT <- typeOf expr
      -- check mutability of exprT here?
      blockT <- typeOf block
      case exprT == blockT of
        True -> return exprT
        False -> throwErrorC ["Attempted to assign a value of type `"
                             , show blockT, "' to an expression of "
                             , "type `", show exprT, "'"]
    While cond block -> do
      condT <- typeOf cond
      case condT == boolT of
        True -> typeOf block
        False -> throwErrorC ["Type of while condition must be a bool, "
                             , "but instead it's a `", show condT, "'"]
    For (Var name) container block -> do
      -- need to make sure container contains things...
      contT <- typeOf container
      -- we probably want to do this via traits instead of this, but eh...
      case contT of
        TApply typ typ' -> setType name typ' >> typeOf block
        typ -> throwErrorC ["Non-container type `", show typ
                           , "' used in a for loop"]

getTable = get <!> table

-- | self-explanatory
push, pop :: Typing ()
push = modify $ \s -> s {table = mempty : table s}
pop = modify $ \s -> s {table = tail $ table s}

setType :: Name -> Type -> Typing Type
setType name typ = do
  tbl:tbls <- getTable
  modify $ \s -> s {table = M.insert name typ tbl : tbls}
  return typ

-- obviously a prototype here...
unusedTypeVar :: Typing Type
unusedTypeVar = return $ TVar "a" 

log :: String -> Typing ()
log = lift . lift . putStrLn
log' = concat ~> log

lookup, lookup1 :: Name -> Typing (Maybe Type)
-- | local lookup, searches head of table list
lookup1 name = getTable >>= head ~> M.lookup name ~> return
-- | recursive lookup, searches up through all symbol tables
lookup name = getTable >>= loop
  where loop [] = return Nothing
        loop (tbl:tbls) = case M.lookup name tbl of Just typ -> return (Just typ)
                                                    Nothing -> loop tbls
lookupAndError :: Name -> Typing Type
lookupAndError name = lookup name >>= \case
  Just typ -> return typ
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

throwErrorC = throwError1 . concat
throwError1 = throwError . pure
addError msg msgs = throwError $ msg : msgs
addError' = addError . concat

builtIns = M.fromList [ ("+", nnn), ("-", nnn), ("*", nnn), ("/", nnn)
                      , ("%", nnn), (">", nnb), ("<", nnb), (">=", nnb)
                      , ("<=", nnb), ("==", nnb), ("!=", nnb)
                      , ("print", numT ==> unitT)
                      , ("Just", TVar "a" ==> TApply (TConst "Maybe") (TVar "a"))
                      , ("Nothing", TApply (TConst "Maybe") (TVar "a")) ]
  where nnn = numT ==> numT ==> numT
        nnb = numT ==> numT ==> boolT
defaultState = TypingState {table = [builtIns], aliases = mempty}

runTyping :: Typable a => a -> (Either TypingError Type, TypingState)
runTyping a = unsafePerformIO $ runStateT (runErrorT $ typeOf a) defaultState

typeIt :: String -> (Either TypingError Type, TypingState)
typeIt input = case grab input of
  Left err -> error $ show err
  Right block -> runTyping block
