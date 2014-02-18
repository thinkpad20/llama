{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
                               , freshName :: Name } deriving (Show)
type Typing = ErrorT TypingError (StateT TypingState IO)

instance Error TypingError where
  strMsg = TE . pure

instance Show TypingError where
  show (TE msgs) = msgs ! concatMap ((++ "\n") . indentBy 4 . trim) ! line

instance Render TypingState where
  render state = line $ concat ["Table: ", render $ init $ table state, ", "
                               , "Aliases: ", render $ aliases state]

instance Render [M.Map Name Type] where
  render mps = line $ "[" ++ (intercalate ", " $ map render mps) ++ "]"

instance Render (M.Map Name Type) where
  render mp = line $ "{" ++ intercalate ", " pairs ++ "}" where
    pairs = mp ! M.toList ! map (\(n, t) -> n ++ ": " ++ render t)

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
  log' ["determining the type of ", render func, " applied to ", render arg]
  funcType <- typeOf func
  argType <- typeOf arg
  log' ["determined funcType to be `", render funcType
       , "' and argType to be `", render argType, "'"]
  case funcType of
    TFunction argType' returnType -> do
      unify argType argType' `catchError` uniError argType argType'
      refine returnType
    -- here we can check if its type is Functionable?
    _ -> throwErrorC ["Expression `", render func, "' is not a function, "
                     , "but of type `", render funcType, "'"]
  where uniError typ typ' = addError' ["`", render func, "' takes `", render typ'
                                      , "' as its argument, but `", render arg
                                      , "' has the type `", render typ, "'"]

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
      Tuple vals -> tTuple <$> mapM typeOf vals
      Lambda argsAndBodies -> do
        -- get all of the arguments and bodies
        t:types <- mapM typeOf argsAndBodies
        -- make sure they're all the same
        mapM_ (unify t) types `catchError` altError
        refine t >>= generalize
      Dot a b -> typeOf (Apply b a)
      Apply a b -> typeOfApply typeOf a b
      _ -> error $ "we can't handle expression `" ++ render expr ++ "'"
    err = addError' ["When typing the expression `", render expr, "'"]
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
    log' ["instantiating typed variable ", name, " as ", render typ]
    instantiate typ >>= setType name
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
  typeOf block = push *> go `catchError` err <* pop
    where
      err = addError' ["When typing the block `", render block, "'"]
      go = case block of
        [] -> return unitT -- shouldn't encounter this, but...
        Break:_ -> return unitT
        [stmt] -> typeOf stmt
        (Return expr):_ -> typeOf expr
        (If' expr blk1):blk2 | any isReturn blk1 -> do
          blk1Type <- typeOf blk1
          blk2Type <- typeOf blk2
          case blk1Type == blk2Type of
            True -> return blk1Type
            False -> throwErrorC ["Two branches have different types: `"
                                 , render blk1Type, "' and `", render blk1Type, "'"]
        stmt:block -> typeOf stmt *> typeOf block
      isReturn (Return _) = True
      isReturn _ = False

instance Typable Statement where
  typeOf statement = go `catchError` err where
    err = case statement of
      Expr expr -> throwError
      _ -> addError' ["When typing the statement `", render statement, "'"]
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
          TConst name [typ'] -> setType name typ' >> typeOf block
          typ -> throwErrorC ["Non-container type `", render typ
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
log s = return ()-- lift . lift . putStrLn
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
                      , ("Just", a ==> TConst "Maybe" [a])
                      , ("Nothing", TConst "Maybe" [a]) ]
  where nnn = numT ==> numT ==> numT
        nnb = numT ==> numT ==> boolT
        [a, b, c] = TVar Polymorphic <$> ["a", "b", "c"]
        (ab, bc, ac) = (a ==> b, b ==> c, a ==> c)

-- | adds to the type aliases map any substitutions needed to make
-- its two arguments equivalent. Throws an error if such a substitution
-- is impossible (todo: throw an error if there's a cycle)
unify type1 type2 | type1 == type2 = return ()
                  | otherwise =
  case (type1, type2) of
    (TVar Polymorphic name, typ) -> addTypeAlias name typ
    (typ, TVar Polymorphic name) -> addTypeAlias name typ
    (TConst name ts, TConst name' ts')
      | name == name' -> mapM_ (uncurry unify) $ zip ts ts'
    _ -> throwErrorC ["Incompatible types: `", render type1, "' and `", render type2, "'"]
  where
    operandsError which = addError' ["When unifying the", which, "operands of `"
                                    , render type1, "' and `", render type2]

-- | takes a type and replaces any type variables in the type with unused
-- variables. Note: in Hindley-Milner, there are two distinct types, Type and
-- Polytype, and instantiate maps between them. Tentatively, we don't need this
-- distinction.
instantiate :: Type -> Typing Type
instantiate typ = fst <$> runStateT (inst typ) mempty where
  inst :: Type -> StateT (M.Map Name Type) Typing Type
  inst typ = case typ of
    TVar Rigid name -> return typ
    TVar Polymorphic name -> do
      M.lookup name <$> get >>= \case
        -- if we haven't yet seen this variable, create a new one
        Nothing -> do typ' <- lift unusedTypeVar
                      modify $ M.insert name typ'
                      return typ'
        -- otherwise, return what we already created
        Just typ' -> return typ'
    TConst name types -> TConst name <$> mapM inst types
    TFunction a b -> TFunction <$$ inst a <*> inst b

-- | the opposite of instantiate; it "polymorphizes" the rigid type variables
-- so that they can be polymorphic in future uses.
generalize :: Type -> Typing Type
generalize typ = case typ of
  TVar Rigid name -> return $ TVar Polymorphic name
  TVar Polymorphic name -> return typ
  TConst name types -> TConst name <$> mapM generalize types
  TFunction a b -> TFunction <$$ generalize a <*> generalize b

-- | follows the type aliases and returns the fully qualified type (as
-- qualified as possible)
refine typ = look mempty typ where
  look :: S.Set Name -> Type -> Typing Type
  look seenNames typ = case typ of
    TConst name types -> TConst name <$> mapM (look seenNames) types
    TVar Rigid _ -> return typ
    TVar Polymorphic name
      | name `S.member` seenNames -> throwError1 "Cycle in type aliases"
      | otherwise -> do
      M.lookup name <$> getAliases >>= \case
        Nothing -> return typ
        Just typ' -> look (S.insert name seenNames) typ'
    TFunction a b -> TFunction <$$ look seenNames a <*> look seenNames b

defaultState = TypingState { table = [builtIns]
                           , aliases = mempty
                           , freshName = "a0"}

-- NOTE: using unsafePerformIO for testing purposes only. This will
-- all be pure code in the end.
runTyping :: Typable a => a -> (Either TypingError Type, TypingState)
runTyping a = unsafePerformIO $ runStateT (runErrorT $ typeOf a) defaultState

typeIt :: String -> IO ()
typeIt input = case grab input of
  Left err -> error $ show err
  Right block -> case runTyping block of
    (Left err, _) -> error $ show err
    (Right block, state) -> putStrLn $ render block ++ "\n" ++ render state
