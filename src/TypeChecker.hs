{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeChecker ( Typable(..), Typing, TypeTable, TypingState(..)
                   , runTyping, runTypingWith, defaultTypingState
                   , typeIt, unifyIt, runTypeChecker, testInstantiate
                   , generalize') where

import Prelude hiding (lookup, log)
import System.IO.Unsafe
import Control.Monad.Error.Class
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Common
import AST
import TypeLib
import Parser (grab, grabT)

class Typable a where
  typeOf :: TypeOf a

type TypeOf a = a -> Typing (Type, Subs)
type NameSpace = [Name]
data TypingState = TypingState { aliases :: M.Map Name Type
                               , nameSpace :: [Name]
                               , typeEnv :: TypeEnv
                               , freshName :: Name } deriving (Show)
type Typing = ErrorT ErrorList (StateT TypingState IO)

defaultTypingState :: TypingState
defaultTypingState = TypingState { aliases = mempty
                                 , nameSpace = []
                                 , typeEnv = builtIns
                                 , freshName = "a0"}

instance Render TypingState where
  render state =
    let (TE env) = typeEnv state
        bi = (\(TE b) -> b) builtIns
        env' = TE $ M.filterWithKey (\k _ -> M.notMember k bi) env in
    line $ mconcat ["Names: ", render env']

instance Render [M.Map Name Type] where
  render mps = line $ "[" <> (T.intercalate ", " $ map render mps) <> "]"

instance Render NameSpace where
  render = T.intercalate "/" . reverse

unifyAdd :: Type -> Type -> Typing ()
unifyAdd t1 t2 = unify (t1, t2) >>= toList ~> mapM_ (uncurry addTypeAlias)

addTypeAlias :: Name -> Type -> Typing ()
addTypeAlias name typ =
  modify $ \s -> s { aliases = M.insert name typ (aliases s)}

applyToEnv :: Subs -> Typing ()
applyToEnv subs = modify $ \s -> s {typeEnv = apply subs (typeEnv s)}

instance Typable Expr where
  typeOf e = case e of
    Number _ -> only numT
    String _ -> only strT
    Constructor name -> only =<< lookupAndInstantiate name
    Var name -> only =<< lookupAndInstantiate name
    Lambda param body -> do
      pushNameSpace "%l"
      (paramT, paramS) <- litTypeOf param
      applyToEnv paramS
      (bodyT, bodyS) <- typeOf body
      popNameSpace
      return (paramT ==> bodyT, paramS <> bodyS)
    Define name expr -> typeOfDefinition name expr
    Extend name expr -> typeOfDefinition name expr
    Apply func arg -> typeOfApply typeOf func arg
    If c t f -> typeOfIf (c, t, f)
    Array (ArrayLiteral arr) -> do
      (ts, subs) <- typeOfList typeOf arr
      case ts of
        [] -> do newT <- unusedTypeVar
                 return (arrayOf newT, subs)
        t:ts -> if all (== t) ts
                then return (arrayOf t, subs)
                else throwError1 "Multiple types in array literal"

    Tuple exprs -> typeOfTuple typeOf exprs
    expr -> throwErrorC ["Can't handle ", render expr]
    where only t = return (t, mempty)

typeOfTuple :: TypeOf Expr -> [Expr] -> Typing (Type, Subs)
typeOfTuple f exprs = do
  (ts, subs) <- typeOfList f exprs
  return (tTuple ts, subs)

typeOfList :: TypeOf Expr -> [Expr] -> Typing ([Type], Subs)
typeOfList f exprs = go ([], mempty) exprs where
  go (ts, subs) [] = return (reverse ts, subs)
  go (ts, subs) (e:es) = do
    (t, s) <- f e
    applyToEnv s
    go (t:ts, s <> subs) es

typeOfDefinition :: Name -> Expr -> Typing (Type, Subs)
typeOfDefinition name expr = lookup1 name >>= \case
  Just _ -> throwErrorC [name, " is already defined in scope"]
  Nothing -> do
    nameT <- unusedTypeVar
    store name (Polytype [] nameT)
    pushNameSpace name
    (type_, subs2) <- typeOf expr
    popNameSpace
    subs3 <- unify (nameT, apply subs2 type_)
    store name =<< generalize (apply subs3 nameT)
    return (nameT, subs3 <> subs2)

typeOfIf :: (Expr, Expr, Expr) -> Typing (Type, Subs)
typeOfIf (cond, true, false) = do
  (condT, condS) <- typeOf cond
  subs1 <- unify (condT, boolT)
  applyToEnv subs1
  (trueT, trueS) <- typeOf true
  applyToEnv trueS
  (falseT, falseS) <- typeOf false
  applyToEnv falseS
  subs2 <- unify (trueT, falseT)
  let subs = mconcat [subs2, falseS, trueS, subs1, condS]
  return (trueT, subs)

-- | Similarly, inferring the type of an application is slightly
-- different when looking for a literal type, so we provide which typeOf
-- function to apply as an argument to typeOfApply
typeOfApply :: TypeOf Expr -> Expr -> Expr -> Typing (Type, Subs)
typeOfApply f func arg = do
  (argT, subs1) <- f arg
  applyToEnv subs1
  (funcT, subs2) <- f func
  retT <- unusedTypeVar
  subs3 <- unify (funcT, argT ==> retT) `catchError` uniError
  return (retT, mconcat [subs1, subs2, subs3])
  where uniError = addError' ["When attempting to apply `", render func
                             , " to argument ", render arg]

instance Typable Block where
  -- Returns the type of the last statement. Operates in a new context.
  typeOf block = do
    log' ["typing the block `", render block, "'"]
    go `catchError` err
    where
      err = addError' ["When typing the block `", render block, "'"]
      go = case block of
        [] -> only unitT -- shouldn't encounter this, but...
        (Break expr):_ -> typeOf expr
        [expr] -> typeOf expr
        (Return expr):_ -> typeOf expr
        expr:block' -> do
          log' ["starting"]
          (_, subs) <- typeOf expr
          applyToEnv subs
          log' ["got these subs: ", render subs]
          typeOf block'

only :: Type -> Typing (Type, Subs)
only t = return (t, mempty)

-- | @litType@ is for expressions from which a "literal type" can be
-- determined; that is, context-free. For example @[1,2,3]@ is literally
-- @[Number]@ regardless of context, and @True@ is always @Bool@.
litTypeOf :: Expr -> Typing (Type, Subs)
litTypeOf expr = case expr of
  Var name -> do
    newT <- unusedTypeVar
    store name (Polytype [] newT)
    only newT
  Typed (Var name) type_ -> do
    store name $ Polytype [] type_
    only type_
  Number _ -> only numT
  String _ -> only strT
  Tuple exprs -> typeOfTuple litTypeOf exprs
  Array (ArrayLiteral arr) -> do
    (ts, subs) <- typeOfList litTypeOf arr
    case ts of
      [] -> only =<< (arrayOf <$> unusedTypeVar)
      (t:ts') | all (== t) ts' -> return (arrayOf t, subs)
             | otherwise -> throwError1 "multiple types in array"
  Constructor name -> only =<< lookupAndInstantiate name
  Apply a b -> typeOfApply litTypeOf a b
  _ -> throwErrorC [ "`", render expr, "' does not have a literal type. "
                   , "Please provide the type of the expression."]

getAliases :: Typing (M.Map Name Type)
getAliases = get <!> aliases

pushNameSpace :: Name -> Typing ()
pushNameSpace name = modify $ \s -> s { nameSpace = name : nameSpace s }

popNameSpace :: Typing ()
popNameSpace = modify $ \s -> s { nameSpace = tail $ nameSpace s }

store :: Name -> Polytype -> Typing ()
store name ptype = do
  nsName <- fullName name
  env <- get <!> typeEnv
  let mod s = s {typeEnv = addToEnv nsName ptype (typeEnv s)}
  modify mod

unusedTypeVar :: Typing Type
unusedTypeVar = do
  -- get the current state
  var <- get <!> freshName
  -- increment the freshName
  modify $ \s -> s { freshName = next var }
  -- wrap it in a type variable and return it
  return $ TVar var
  where
    next :: Name -> Name
    next n = let name = T.unpack n
                 (c:cs) = reverse name in
      T.pack $ if c < '9' then reverse $ succ c : cs
               else if (head name) < 'z' then (succ $ head name) : "0"
               else map (\_ -> 'a') name <> "0"

-- | Local lookup, searches head of table list.
lookup1 :: Name -> Typing (Maybe Polytype)
lookup1 name = do
  (TE env) <- get <!> typeEnv
  return $ M.lookup name env

-- | Recursive lookup, searches up through all symbol tables.
lookup :: Name -> Typing (Maybe Polytype)
lookup name = do
  ns <- get <!> nameSpace
  loop ns
  where loop [] = lookup1 name
        loop (n:ns) = lookup1 (render $ name:n:ns) >>= \case
          Just typ -> return (Just typ)
          Nothing -> loop ns

lookupAndInstantiate :: Name -> Typing Type
lookupAndInstantiate name = lookup name >>= \case
  Just typ -> instantiate typ
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

unify :: (Type, Type) -> Typing Subs
unify types = case types of
  (TVar name, typ) -> bind name typ
  (typ, TVar name) -> bind name typ
  (TConst n, TConst n') | n == n' -> return mempty
  (TTuple ts, TTuple ts') | length ts == length ts' ->
    mconcat <$> mapM unify (zip ts ts')
  (TFunction a b, TFunction a' b') -> do
    subs1 <- unify (a, a')
    subs2 <- unify (apply subs1 b, apply subs1 b')
    return (subs1 <> subs2)
  (TApply a b, TApply a' b') -> do
    subs1 <- unify (a, a')
    subs2 <- unify (apply subs1 b, apply subs1 b')
    return (subs1 <> subs2)
  (TMultiFunc set, TFunction from to) -> unifyMultiMtoF set from to
  (type1, type2) -> throwErrorC $ [ "Incompatible types: `", render type1
                                  , "' and `", render type2, "'"]

bind :: Name -> Type -> Typing Subs
bind name typ = case typ of
  TVar n | n == name -> return mempty
  _ -> if name `S.member` free typ then occursCheck
       else return (Subs $ M.singleton name typ)
  where occursCheck = throwErrorC ["Occurs check"]

unifyMultiMtoF :: TypeMap -> Type -> Type -> Typing Subs
unifyMultiMtoF set from to = do
    -- Go through the multifunction's argument types. If we can
    -- unify @from@ with an argument type, record its subs and
    -- pair it with the @to@ type that argument type maps to.
    -- Valid choices get wrapped in a @Just@, otherwise @Nothing@.
    subsTos <- forM (M.toList set) $ \(from', to') -> do
      log' ["trying to unify `", render from', "' with `", render from, "'"]
      fmap (wrap to') (unify (from', from)) `catchError` skip
    -- Filter out the @Nothing@ values and sort by smallest subs.
    let valids = catMaybes subsTos ! sortWith (fst ~> size)
    log' ["valids: ", render valids]
    when (length valids == 0) $ throwError1 "no argument matches"
    tryAll mempty valids
  where
    skip _ = return Nothing
    wrap type_ subs = Just (subs, type_)
    tryAll subs valids = case valids of
      -- If there aren't any valids, it's incompatible.
      [] -> throwError1 "no return type matches"
      -- If there's exactly one, try to unify @to@ with it after adding the subs.
      [(subs', to')] -> do
        newSubs <- unify (apply (subs <> subs') to', apply (subs <> subs') to)
        return (newSubs <> subs' <> subs)
      -- If the first two are of equal size, then it's ambiguous, an error.
      (s1, _):(s2, _):_ | size s1 == size s2 -> ambiguousErr s1 s2
      -- Otherwise, try to unify, and if it fails, move on to the next.
      (subs', to'):rest -> do
        -- Create new types by applying substitutions to existing.
        let newTo  = apply (subs <> subs') to
            newTo' = apply (subs <> subs') to'
        newSubs <- unify (newTo', newTo) `catchError` \_ -> tryAll subs rest
        return (newSubs <> subs' <> subs)
    ambiguousErr s1 s2 = let (type1, type2) = (TMultiFunc set, TFunction from to) in
      throwErrorC [
        "Ambiguous application of `", render type1, "' to `", render type2, "'. "
      , "Multiple unification choices are equally valid: could be `", render s1
      , "', or `", render s2, "'"
      ]

-- | Takes a polytype and replaces any type variables in the type with unused
-- variables.
instantiate :: Polytype -> Typing Type
instantiate (Polytype names type_) = do
  subs <- fromList <$> forM names (\name -> (,) name <$> unusedTypeVar)
  return $ apply subs type_

generalize :: Type -> Typing Polytype
generalize typ = generalize' <$$ fmap typeEnv get <*> pure typ

generalize' :: TypeEnv -> Type -> Polytype
generalize' env type_ =
  let vars = S.toList $ free type_ S.\\ free env
      next name = case T.last name of
        c | c < 'z' -> T.init name `T.snoc` succ c
          | True    -> name `T.snoc` 'a'
      newName _ [] = []
      newName name (old:rest) = (old, TVar name) : newName (next name) rest
      subs = newName "a" vars
      newNames = map (snd ~> (\(TVar name) -> name)) subs
  in Polytype newNames $ apply (fromList subs) type_

-- | Follows the type aliases and returns the fully qualified type (as
-- qualified as possible)
refine :: Type -> Typing Type
refine typ = fst <$> runStateT (look typ) mempty where
  look typ' = case typ' of
    TConst _ -> return typ'
    TRigidVar _ -> return typ'
    TMut typ'' -> TMut <$> look typ''
    TVar name -> do
      seenNames <- get
      if name `S.member` seenNames then throwError1 "Cycle in type aliases"
      else do
        M.lookup name <$> (lift getAliases) >>= \case
          Nothing -> return typ
          Just typ'' -> modify (S.insert name) >> look typ''
    TFunction a b -> TFunction <$$ look a <*> look b
    TApply a b -> TApply <$$ look a <*> look b
    TTuple ts -> TTuple <$> mapM look ts

testInstantiate :: Polytype -> Either ErrorList Type
testInstantiate p = case fst $ runTypeChecker $ instantiate p of
  Left err -> Left $ ErrorList [render err]
  Right type_ -> Right type_

fullName :: Name -> Typing T.Text
fullName name = get <!> nameSpace <!> (name:) <!> render

-- NOTE: using unsafePerformIO for testing purposes only. This will
-- all be pure code in the end.
runTypingWith :: Typable a => TypingState -> a -> (Either ErrorList Type, TypingState)
runTypingWith state a = unsafePerformIO $ runStateT (runErrorT $ t a) state
  where t a = do (t, s) <- typeOf a
                 return $ normalize (apply s t)

runTyping :: Typable a => a -> (Either ErrorList Type, TypingState)
runTyping = runTypingWith defaultTypingState

runUnify :: Type -> Type -> (Either ErrorList Subs, TypingState)
runUnify type1 type2 =
  unsafePerformIO $ runStateT (runErrorT $ unify (type1, type2)) defaultTypingState

runTypeChecker :: Typing a => (Either ErrorList a, TypingState)
runTypeChecker typing =
  unsafePerformIO $ runStateT (runErrorT typing) defaultTypingState

typeIt :: String -> Either ErrorList Type
typeIt input = case grab input of
  Left err -> Left $ ErrorList ["Parse error:\n" <> (T.pack $ show err)]
  Right block -> fst $ runTyping block

unifyIt :: (String, String) -> Either ErrorList Subs
unifyIt (input1, input2) = do
  type1 <- grabT input1
  type2 <- grabT input2
  fst $ runUnify type1 type2

log :: T.Text -> Typing ()
log s = if hideLogs then return () else lift2 $ putStrLn $ T.unpack s
log' = mconcat ~> log

lift2 = lift . lift


hideLogs = True
