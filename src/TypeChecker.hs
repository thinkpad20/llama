{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TypeChecker ( Typable(..), Typing, TypeTable, TypingState(..)
                   , runTyping, runTypingWith, defaultTypingState
                   , typeIt, unifyIt, runTypeChecker, testInstantiate
                   , generalize') where

import Prelude (IO, Eq(..), Ord(..), Bool(..),
                Double, String, Maybe(..), Int, Monad(..),
                ($), (.), floor, map, Functor(..), mapM,
                (+), (-), elem, Either(..), Char, last,
                otherwise, (=<<), Read(..), error, foldl,
                foldr, foldr1, all, reverse, any, zip, succ,
                head, length, flip, fst, span, snd,
                undefined)
import System.IO.Unsafe
import Control.Monad.Error.Class
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Prelude as P
import Data.List (nub)

import Common
import AST
import TypeLib
import Parser (grab, grabT)
import Desugar (desugarIt)

addTypeAlias :: Name -> Type -> Typing ()
addTypeAlias name typ =
  modify $ \s -> s { aliases = M.insert name typ (aliases s)}

applyToEnv :: Subs -> Typing ()
applyToEnv subs = modify $ \s -> s {typeEnv = apply subs (typeEnv s)}

instance Typable Expr where
  typeOf e = case e of
    Block blk -> pushNameSpace "%b" *> typeOf blk <* popNameSpace
    Number _ -> only numT
    String _ -> only strT
    Constructor name -> only =<< lookupAndInstantiate name
    Var name -> only =<< lookupAndInstantiate name
    Case expr argsBodies -> typeOfCase expr argsBodies
    Lambda param body -> do
      pushNameSpace "%l"
      (paramT, paramS) <- litTypeOf param
      applyToEnv paramS
      (bodyT, bodyS) <- typeOf body
      popNameSpace
      return (paramT ==> bodyT, paramS <> bodyS)
    Define name expr -> typeOfDefinition name expr
    Extend name expr -> typeOfExtension name expr
    Apply func arg -> typeOfApply typeOf func arg
    Dot a b -> typeOfDot typeOf a b
    If c t f -> typeOfIf c t f
    For init cond step expr -> typeOfFor init cond step expr
    ObjDec dec -> handleObjDec dec >> only unitT
    Literal (ArrayLiteral arr) -> do
      (ts, subs) <- typeOfList typeOf arr
      case ts of
        [] -> do newT <- unusedTypeVar
                 return (arrayOf newT, subs)
        t:ts -> if all (== t) ts
                then return (arrayOf t, subs)
                else throwError1 "Multiple types in array literal"

    Tuple exprs kw -> typeOfTuple typeOf exprs kw
    expr -> throwErrorC ["Can't handle ", show expr, " or needs desugaring"]
    where only t = return (t, mempty)

typeOfTuple :: TypeOf Expr -> [Expr] -> Kwargs -> Typing (Type, Subs)
typeOfTuple f exprs _ = do
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

-- | TODO: DRY this up
typeOfExtension :: Name -> Expr -> Typing (Type, Subs)
typeOfExtension name expr = lookup1 name >>= \case
  Nothing -> throwErrorC [name, " is not defined in immediate scope"]
  Just polytype@(Polytype _ t) -> case t of
    TFunction _ _ -> extend polytype
    TMultiFunc  _ -> extend polytype
    type_ ->
      throwErrorC ["Can't extend non-function type `", render type_, "'"]
  where
    extend polytype = do
      newT <- unusedTypeVar
      pushNameSpace name
      (newT', subs2) <- typeOf expr
      popNameSpace
      case newT' of
        -- Make sure it's a function type.
        TFunction from to ->
          clashes polytype from ! \case
            -- Make sure it doesn't overwrite anything.
            False -> do
              subs3 <- unify (newT, apply subs2 newT')
              generalized <- generalize (apply subs3 newT)
              store name (generalized <> polytype)
              return (newT, subs3 <> subs2)
            True -> throwErrorC [ "Declared function extension "
                                    , "would overwrite existing "
                                    , "version for type `", render from, "'"]
        _ -> throwErrorC [ "Only functions can be extended, "
                         , "and only with other functions"]
    -- | TODO: "normalize" isn't a super-intelligent function. We might want
    -- to use a generalized type or something.
    clashes (Polytype _ t) from = case t of
      TFunction from' _ -> normalize from == normalize from'
      TMultiFunc set ->
        any (\typ -> normalize typ == normalize from) (M.keys set)


typeOfCase :: Expr -> [(Expr, Expr)] -> Typing (Type, Subs)
typeOfCase expr patsBodies = do
  (exprT, exprS) <- typeOf expr
  applyToEnv exprS
  -- Give each pattern and body a number, for separate namespaces
  results <- forM (zip [0..] patsBodies) $ \(n, (pat, body)) -> do
    -- Grab a copy of the type env
    pushNameSpace ("%case" <> show n)
    (patT, patS) <- litTypeOf pat
    patS' <- unify (patT, exprT) `catchError` addError "pattern type mismatch"
    applyToEnv patS
    (bodyT, bodyS) <- typeOf body
    popNameSpace
    applyToEnv bodyS
    return (bodyT, mconcat [exprS, patS, patS', bodyS])
  unify' results
  where unify' ((t, subs):rest) = go (t, subs) rest
        go (t, subs) [] = return (t, subs)
        go (t, subs) ((t', subs'):rest) = do
          subs' <- unify (t, t') `catchError` addError "case result type mismatch"
          go (t, subs <> subs') rest

typeOfIf :: Expr -> Expr -> Expr -> Typing (Type, Subs)
typeOfIf cond true false = do
  (condT, condS) <- typeOf cond
  subs1 <- unify (condT, boolT)
  applyToEnv (subs1 <> condS)
  (trueT, trueS) <- typeOf true
  applyToEnv trueS
  (falseT, falseS) <- typeOf false
  applyToEnv falseS
  subs2 <- unify (trueT, falseT)
  let subs = mconcat [subs2, falseS, trueS, subs1, condS]
  return (trueT, subs)

typeOfFor :: Expr -> Expr -> Expr -> Expr -> Typing (Type, Subs)
typeOfFor init cond step expr = do
  (condT, condS) <- typeOf cond
  subs <- unify (condT, boolT)
  applyToEnv (subs <> condS)
  (exprT, exprS) <- typeOf expr
  return (maybeT exprT, condS <> exprS <> subs)

-- | Similarly, inferring the type of an application is slightly
-- different when looking for a literal type, so we provide which typeOf
-- function to apply as an argument to typeOfApply
typeOfApply :: TypeOf Expr -> Expr -> Expr -> Typing (Type, Subs)
typeOfApply f func arg = do
  log' ["typing ", render func, " applied to ", render arg]
  (argT, subs1) <- f arg
  log' ["typing arg got type ", render argT, " subs ", render subs1]
  applyToEnv subs1
  (funcT, subs2) <- f func
  log' ["typing func got type ", render funcT, " subs ", render subs2]
  applyToEnv subs2
  retT <- unusedTypeVar
  subs3 <- unify (funcT, argT ==> retT) `catchError` uniError
  log' ["unifying ", render funcT, " with ", render (argT ==> retT), " got subs ", render subs3]
  return (retT, mconcat [subs1, subs2, subs3])
  where uniError = addError' ["When attempting to apply `", render func
                             , " to argument ", render arg]

typeOfDot :: TypeOf Expr -> Expr -> Expr -> Typing (Type, Subs)
typeOfDot f a b = do
  (aT, aS) <- f a
  case b of
    Var name ->
      aT `getAttribute` name >>= \case
        Just t -> return (t, aS)
        Nothing -> typeOfApply f b a
    _ -> typeOfApply f b a

getAttribute :: Type -> Name -> Typing (Maybe Type)
getAttribute _ _ = return Nothing

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
          log' ["doing ", render expr]
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
    -- TODO: find a way to "rigidify" this type, so that the (a->a)/(a->b)
    -- unification fails as it should (basically, type variables in type
    -- signatures should be treated as constants).
    store name $ Polytype [] type_
    only type_
  Typed WildCard type_ -> only type_
  Number _ -> only numT
  String _ -> only strT
  Tuple exprs kw -> typeOfTuple litTypeOf exprs kw
  Literal (ArrayLiteral arr) -> do
    (ts, subs) <- typeOfList litTypeOf arr
    case ts of
      [] -> only =<< (arrayOf <$> unusedTypeVar)
      (t:ts') | all (== t) ts' -> return (arrayOf t, subs)
             | otherwise -> throwError1 "multiple types in array"
  Constructor name -> only =<< lookupAndInstantiate name
  Apply a b -> typeOfApply litTypeOf a b
  WildCard -> only =<< unusedTypeVar
  _ -> throwErrorC [ "`", render expr, "' does not have a literal type. "
                   , "Please provide the type of the expression."]

getAliases :: Typing (M.Map Name Type)
getAliases = get <!> aliases

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
  where loop (NameSpace []) = lookup1 name
        loop (NameSpace (n:ns)) = lookup1 (render $ NameSpace $ name:n:ns) >>= \case
          Just typ -> return (Just typ)
          Nothing -> loop $ NameSpace ns

lookupAndInstantiate :: Name -> Typing Type
lookupAndInstantiate name = lookupAndError name >>= instantiate

lookupAndError :: Name -> Typing Polytype
lookupAndError name = lookup name >>= \case
  Just typ -> return typ
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

unify :: (Type, Type) -> Typing Subs
unify types = log' ["unifying ", render types] >> case types of
  (TVar name, typ) -> log "case 1" >> bind name typ
  (typ, TVar name) -> log "case 2" >> bind name typ
  (TConst n, TConst n') | n == n' -> return mempty
  (TTuple ts kw, TTuple ts' kw') -> unifyTuple (ts, kw) (ts', kw')
  (TTuple ts kw, typ) -> unifyTuple (ts, kw) ([typ], mempty)
  (typ, TTuple ts kw) -> unifyTuple (ts, kw) ([typ], mempty)
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

unifyTuple :: ([Type], TKwargs) -> ([Type], TKwargs) -> Typing Subs
unifyTuple (ts, _) (ts', _)
  | length ts /= length ts' = throwError1 "Tuples of different lengths"
  | otherwise = go mempty ts ts'
  where go subs [] _ = return subs
        go subs (t:ts) (t':ts') = do
          subs' <- unify (apply subs t, apply subs t')
          go (subs' <> subs) ts ts'

unifyMultiMtoF :: TypeMap -> Type -> Type -> Typing Subs
unifyMultiMtoF set from to = do
    -- Go through the multifunction's argument types. If we can
    -- unify @from@ with an argument type, record its subs and
    -- pair it with the @to@ type that argument type maps to.
    -- Valid choices get wrapped in a @Just@, otherwise @Nothing@.
    subsTos <- forM (M.toList set) $ \(from', to') -> do
      log' ["trying to unify `", render from', "' with `", render from, "'"]
      flip catchError skip $ do
        subs <- unify (from', from)
        log' ["success! subs are: ", render subs]
        return $ Just (subs, to)
      fmap (wrap to') (unify (from', from) <* log' ["success!"]) `catchError` skip
    -- Filter out the @Nothing@ values and sort by smallest subs.
    let valids = catMaybes subsTos ! sortWith (fst ~> size)
    log' ["valids: ", render valids]
    when (length valids == 0) $ throwError1 "no argument matches"
    tryAll mempty valids
  where
    skip _ = log' ["failed to unify"] >> return Nothing
    wrap type_ subs = Just (subs, type_)
    tryAll subs valids = log' ["coming in with subs `", render subs, "'"] >> case valids of
      -- If there aren't any valids, it's incompatible.
      [] -> throwError1 "no return type matches"
      -- If there's exactly one, try to unify @to@ with it after adding the subs.
      [(subs', to')] -> do
        newSubs <- unify (apply (subs <> subs') to', apply (subs <> subs') to)
        return (newSubs <> subs' <> subs)
      -- If the first two are of equal size:
      (subs', to'):valids' -> do

        -- Find all of the types of the same degree of ambiguity.
        let (ambigs, rest) = span (\(s, _) -> size subs' == size s) valids'
        log' ["ambigs: ", render ambigs]
        case ambigs of
          -- No ambiguity, can just finish
          [] -> finish subs subs' to' rest
          -- Some ambiguity. See if ambiguity goes away when we look
          -- at the return types.
          _  -> do
            -- New valids: ones that the return type also unifies.
            let subsTos = (subs', to'):ambigs
            vs <- forM subsTos $ \(s, t) ->
              flip catchError skip $ do
                -- @s@ is our incoming subs. See if we can generate
                -- a new set of subs by unifying @to@ with @t@.
                s' <- unify (to, t)
                -- If we can unify, compose @s@ with @s'@ and wrap in @Just@.
                return $ Just (s <> s', t)
            -- Filter out the Nothing values and sort by smallest
            let vs' = catMaybes vs ! sortWith (fst ~> size)
            log' ["subs: ", render subs]
            log' ["vs, vs':", render vs']
            case vs' of
              -- None of these match: keep going
              [] -> tryAll subs rest
              -- One match: we can finish
              [(s, t)] -> finish subs s t rest
              -- More than one: ambiguous, to the max :(
              _ -> ambiguousErr (map fst vs')
    finish subs subs' to' rest = do
      log' [render subs']
      -- Create new types by applying substitutions to existing.
      let newTo  = apply (subs <> subs') to
          newTo' = apply (subs <> subs') to'
      newSubs <- unify (newTo', newTo) `catchError` \_ -> tryAll subs rest
      return (newSubs <> subs' <> subs)
    ambiguousErr subs = let (type1, type2) = (TMultiFunc set, TFunction from to) in
      throwErrorC [
        "Ambiguous application of `", render type1, "' to `", render type2, "'. "
      , "Multiple unification choices are equally valid: could be ", rsubs subs
      ]
    rsubs :: [Subs] -> T.Text
    rsubs  = foldr (\s s' -> "`" <> render s <> "', or `" <> render s' <> "'") ""

-- | Takes a polytype and replaces any type variables in the type with unused
-- variables.
instantiate :: Polytype -> Typing Type
instantiate (Polytype names type_) = do
  subs <- fromList <$> forM names (\name -> (,) name <$> unusedTypeVar)
  return $ apply subs type_

generalize :: Type -> Typing Polytype
generalize typ = generalize' <$> fmap typeEnv get <*> pure typ

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
    TMod modi typ'' -> TMod modi <$> look typ''
    TVar name -> do
      seenNames <- get
      if name `S.member` seenNames then throwError1 "Cycle in type aliases"
      else do
        M.lookup name <$> (lift getAliases) >>= \case
          Nothing -> return typ
          Just typ'' -> modify (S.insert name) >> look typ''
    TFunction a b -> TFunction <$> look a <*> look b
    TApply a b -> TApply <$> look a <*> look b
    TTuple ts kw -> fmap (\ts' -> TTuple ts' kw) $ mapM look ts

testInstantiate :: Polytype -> Either ErrorList Type
testInstantiate p = case fst $ runTypeChecker $ instantiate p of
  Left err -> Left $ ErrorList [render err]
  Right type_ -> Right type_

handleObjDec :: ObjectDec -> Typing ()
handleObjDec (ObjectDec { objName=name, objVars=vars
                        , objConstrs=constrs, objAttrs=attrs}) = do
  -- Make sure type's name is unique.
  whenM (objNameExists name) alreadyExists
  -- Make sure var list has no duplicates
  unless (nub vars == vars) notUnique
  let kind = foldr step (TVar "*") vars
  attribs <- getAttribs attrs
  let fullType = foldl TApply (TConst name) (map TVar vars)
  forM_ constrs $ handleConstructor vars fullType
  registerObject name kind
  where
    step _ = TFunction (TVar "*")
    notUnique = throwErrorC ["Duplicate type variable in list: ", render vars]
    alreadyExists = throwErrorC [ "Object named ", name, " has already been "
                                , "declared in scope"]

objNameExists :: Name -> Typing Bool
objNameExists name = do
  fName <- fullName name
  M.member fName . knownTypes <$> get

-- TODO
getAttribs :: [Expr] -> Typing [(Name, Either Expr Type)]
getAttribs exprs = forM exprs $ \case
  Define name expr -> return (name, Left expr)
  Typed (Var name) type_ -> return (name, Right type_)
  expr -> throwErrorC ["Illegal attribute declaration: ", render expr]

registerObject :: Name -> Kind -> Typing ()
registerObject name kind = do
  fName <- fullName name
  cTypes <- get <!> knownTypes
  modify $ \s -> s {knownTypes = M.insert name kind cTypes}

-- Need to fix this
handleConstructor :: [Name] -> Type -> ConstructorDec -> Typing ()
handleConstructor vars finalType dec = do
  let (name, args) = (constrName dec, constrArgs dec)
  types <- getTypes args
  let type_ = foldr TFunction finalType types
  store name =<< generalize type_
  where getTypes args = forM args $ \case
          Typed (Var _) type_ -> return type_
          Var name -> return $ TVar name
          Constructor name -> return $ TConst name
          expr -> throwErrorC ["Illegal constructor argument: ", render expr]
        notInArgs name = throwErrorC ["Unknown type variable '", name, "'"]
        check type_ = case type_ of
          TVar name | name `elem` vars -> return type_
                    | otherwise -> notInArgs name
          TConst _ -> return type_ -- TODO look this up
          TFunction a b -> TFunction <$> check a <*> check b
          TApply a b -> TApply <$> check a <*> check b
          TTuple ts kw | kw == mempty -> tTuple <$> mapM check ts
          _ -> error $ "Check not implemented: " <> P.show type_

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
typeIt input = case desugarIt input of
  Left err -> Left $ ErrorList ["Parse error:\n" <> show err]
  Right block -> fst $ runTyping block

unifyIt :: (String, String) -> Either ErrorList Subs
unifyIt (input1, input2) = do
  e1 <- grabT input1
  e2 <- grabT input2
  fst (runUnify e1 e2)
