{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
module TypeChecker ( Typing, TypeTable, TypingState(..)
                   , runTyping, runTypingWith, defaultTypingState
                   , typeIt, unifyIt, generalize') where

import Prelude (IO, Eq(..), Ord(..), Bool(..),
                Double, String, Maybe(..), Int, Monad(..),
                ($), (.), floor, map, Functor(..), mapM,
                (+), (-), elem, Either(..), Char, last,
                otherwise, (=<<), Read(..), error, foldl,
                foldr, foldr1, all, reverse, any, zip, succ,
                head, length, flip, fst, span, snd,
                undefined)
import System.IO.Unsafe
import "mtl" Control.Monad.Error.Class
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Prelude as P
import Data.List (nub)

import Common
import AST
import TypeLib
import Parser (grabT)
import Desugar (desugarIt)

typeOf :: Expr -> Typing (Type, Subs)
typeOf e = case e of
  Block blk -> pushNameSpace "%b" *> typeOfBlock blk <* popNameSpace
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
    (types, subs) <- typeOfList typeOf arr
    case types of
      [] -> do newT <- unusedTypeVar
               return (arrayOf newT, subs)
      t:ts -> if all (== t) ts
              then return (arrayOf t, subs)
              else throwError1 "Multiple types in array literal"

  Tuple exprs kw -> typeOfTuple typeOf exprs kw
  TypeDef name type_ -> addTypeAlias name type_ *> only unitT
  expr -> throwErrorC ["Can't handle ", show expr, " or needs desugaring"]

typeOfTuple :: TypeOf Expr -> [Expr] -> Kwargs -> Typing (Type, Subs)
typeOfTuple f exprs kws = do
  (ts, subs) <- typeOfList f exprs
  (kwts, subs') <- typeOfKwargs f kws
  return (TTuple ts kwts, subs' <> subs)

typeOfKwargs :: TypeOf Expr ->
                [(Name, Either Expr Type)] ->
                Typing (TKwargs, Subs)
typeOfKwargs f = go mempty where
  go subs = \case
    [] -> return ([], subs)
    (name, Right t):rest -> do
      -- A kwarg with just a type (which must occur on the left side
      -- of a lambda) gets wrapped in a `Maybe` type
      store name (Polytype [] $ maybeT $ apply subs t)
      (rest', subs') <- go subs rest
      return ((name, t):rest', subs' <> subs)
    (name, Left expr):rest -> do
      (t, subs') <- f expr
      let newSubs = subs <> subs'
      store name (Polytype [] (apply newSubs t))
      (rest', newSubs') <- go newSubs rest
      return ((name, t):rest', newSubs' <> newSubs)

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
        TFunction from _ ->
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
  results <- forM (zip [0::Int ..] patsBodies) $ \(n, (pat, body)) -> do
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
          subs'' <- unify (t, t') `catchError` addError "case result type mismatch"
          go (t, subs'' <> subs' <> subs) rest

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
typeOfFor init cond step body = do
  (_, initS) <- typeOf init
  applyToEnv initS
  (condT, condS) <- typeOf cond
  unifyS <- unify (apply (initS <> condS) condT, boolT)
  applyToEnv (unifyS <> condS)
  (_, stepS) <- typeOf step
  applyToEnv stepS
  (bodyT, bodyS) <- typeOf body
  return (maybeT bodyT, mconcat [bodyS, stepS, unifyS, condS, initS])

-- | Similarly, inferring the type of an application is slightly
-- different when looking for a literal type, so we provide which typeOf
-- function to apply as an argument to typeOfApply
typeOfApply :: TypeOf Expr -> Expr -> Expr -> Typing (Type, Subs)
typeOfApply f func arg = do
  (argT, argS) <- f arg
  (funcT, funcS) <- applyToEnv argS *> f func
  retT <- unusedTypeVar
  unifyS <- unify (apply funcS funcT, argT ==> retT) `catchError` uniError
  return (retT, mconcat [argS, funcS, unifyS])
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

addTypeAlias :: Name -> Type -> Typing ()
addTypeAlias name typ =
  modify $ \s -> s { aliases = M.insert name typ (aliases s)}

applyToEnv :: Subs -> Typing ()
applyToEnv subs = modify $ \s -> s {typeEnv = apply subs (typeEnv s)}

-- | Returns the type of the last statement, or responds to control flow.
typeOfBlock :: Block -> Typing (Type, Subs)
typeOfBlock block = go `catchError` err
  where
    err = addError' ["When typing the block `", render block, "'"]
    go = case block of
      [] -> only unitT
      [expr] -> typeOf expr
      (Break expr):rest -> typeOf expr <* typeOfBlock rest
      (Return expr):rest -> typeOf expr <* typeOfBlock rest
      expr:rest -> do
        (_, subs) <- typeOf expr
        applyToEnv subs
        typeOfBlock rest

only :: Type -> Typing (Type, Subs)
only t = return (t, mempty)

-- | @litType@ is for expressions from which a "literal type" can be
-- determined; that is, context-free. For example @[1,2,3]@ is literally
-- @[Number]@ regardless of context, and @True@ is always @Bool@.
litTypeOf :: Expr -> Typing (Type, Subs)
litTypeOf expr = case expr of
  Var name -> unusedTypeVar >>== store name . plain >>= only
  -- TODO: find a way to "rigidify" this type, so that the (a->a)/(a->b)
  -- unification fails as it should (basically, type variables in type
  -- signatures should be treated as constants).
  Typed (Var name) type_ -> refine type_ >>== store name . Polytype [] >>= only
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
  let mod s = s {typeEnv = addToEnv nsName ptype env}
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
    next n = let name = T.unpack n
                 (c:cs) = reverse name in
      T.pack $ if c < '9' then reverse $ succ c : cs
               else if head name < 'z' then succ (head name) : "0"
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
  (TVar name, typ) -> bind name typ
  (typ, TVar name) -> bind name typ
  (TConst n, TConst n') | n == n' -> return mempty
  (TTuple ts kw, TTuple ts' kw') -> unifyTuple (ts, kw) (ts', kw')
  (TTuple ts kw, typ) -> unifyTuple (ts, kw) ([typ], mempty)
  (typ, TTuple ts kw) -> unifyTuple ([typ], mempty) (ts, kw)
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
unifyTuple = go mempty where
  -- End condition: we ate everything
  go subs ([], []) ([], []) = return subs
  -- Error condition: not enough args given
  go _ (_:_, _) ([], _) = notEnough
  -- Match args left to right
  go subs (t:ts, ks) (t':ts', ks') = do
    subs' <- unify (apply subs t, apply subs t')
    go (subs' <> subs) (ts, ks) (ts', ks')
  -- If we have extra types on the right, match them with kwargs
  go subs ([], (n, t):ks) (t':ts', ks') = do
    subs' <- unify (apply subs t, apply subs t') `catchError` argWithKwarg n
    go (subs' <> subs) ([], ks) (ts', ks')
  -- If we don't have any kwargs to match with, that's an error
  go _ ([], []) (_:_, _) = tooMany
  -- Consume all of the kwargs on the left (provided kwargs)
  go subs ([], ks) ([], (n, t'):ks') = case P.lookup n ks of
    Nothing -> unknownKwarg n
    Just t -> do
      subs' <- unify (apply subs t, apply subs t') `catchError` kwTypeErr t t'
      -- Remove the matching kwarg from the left (and right), and recurse
      go (subs' <> subs) ([], P.filter ((/= n) . fst) ks) ([], ks')
  unknownKwarg name = throwErrorC ["Unknown keyword argument '", name, "'"]
  kwTypeErr t t' = addError' ["When matching in keyword argument: function "
                             , "expects '", render t, "' but '", render t'
                             , "' was given"]
  argWithKwarg name = addError'
    ["When attempting to match kwarg named '", name, "' with positional argument"]
  notEnough = throwError1 "Not enough arguments"
  tooMany = throwError1 "Too many arguments"

  --go subs ([], )
  -- | length ts /= length ts' = throwError1 "Tuples of different lengths"
  -- | otherwise = go mempty ts ts'
  --where go subs [] _ = return subs

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

-- | Finds all free type variables in the type, which are not found in the
-- given environment, and creates a polytype listing those variables.
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

-- | Generalizes against the environment in the TypingState.
generalize :: Type -> Typing Polytype
generalize typ = generalize' <$> fmap typeEnv get <*> pure typ

-- | Takes a polytype and replaces any type variables in the type with unused
-- variables. Opposite of generalize.
instantiate :: Polytype -> Typing Type
instantiate (Polytype names type_) = do
  subs <- fromList <$> forM names (\name -> (,) name <$> unusedTypeVar)
  return $ apply subs type_

-- | Follows the type aliases and returns the fully qualified type (as
-- qualified as possible)
refine :: Type -> Typing Type
refine typ = fst <$> runStateT (look typ) mempty where
  look :: Type -> StateT (S.Set Name) Typing Type
  look typ' = case typ' of
    TConst _ -> return typ'
    TMod modi typ'' -> TMod modi <$> look typ''
    TVar name -> do
      seenNames <- get
      if name `S.member` seenNames then lift $ throwError1 "Cycle in type aliases"
      else do
        M.lookup name <$> (lift getAliases) >>= \case
          Nothing -> return typ
          Just typ'' -> modify (S.insert name) >> look typ''
    TFunction a b -> TFunction <$> look a <*> look b
    TApply a b -> TApply <$> look a <*> look b
    TTuple ts kw -> fmap (\ts' -> TTuple ts' kw) $ mapM look ts

-- | Takes an `object` declaration, adds the kind and attribute information for that
-- object to the environment, as well as creating function signatures for its
-- constructors.
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
  registerObject name kind attribs
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
getAttribs :: [Expr] -> Typing [Attribute]
getAttribs exprs = forM exprs $ \case
  Define name expr -> do
    (exprT, exprS) <- typeOf expr
    when (exprS /= mempty) $
      throwError1 "Cannot capture variables in an attribute"
    return (name, exprT, Just expr)
  Typed (Var name) type_ -> return (name, type_, Nothing)
  expr -> throwErrorC ["Illegal attribute declaration: ", render expr]

registerObject :: Name -> Kind -> [Attribute] -> Typing ()
registerObject name kind attribs = do
  fName <- fullName name
  cTypes <- get <!> knownTypes
  modify $ \s -> s {knownTypes = M.insert fName (kind, attribs) cTypes}

-- Need to fix this
handleConstructor :: [Name] -> Type -> ConstructorDec -> Typing ()
handleConstructor _ finalType dec = do
  let (name, args) = (constrName dec, constrArgs dec)
  types <- getTypes args
  let type_ = foldr TFunction finalType types
  store name (generalize' mempty type_)
  where getTypes args = forM args $ \case
          Typed (Var _) type_ -> return type_
          Var name -> return $ TVar name
          Constructor name -> return $ TConst name
          expr -> throwErrorC ["Illegal constructor argument: ", render expr]
        --notInArgs name = throwErrorC ["Unknown type variable '", name, "'"]
        --check type_ = case type_ of
        --  TVar name | name `elem` vars -> return type_
        --            | otherwise -> notInArgs name
        --  TConst _ -> return type_ -- TODO look this up
        --  TFunction a b -> TFunction <$> check a <*> check b
        --  TApply a b -> TApply <$> check a <*> check b
        --  TTuple ts kw | kw == mempty -> tTuple <$> mapM check ts
        --  _ -> error $ "Check not implemented: " <> P.show type_

-- NOTE: using unsafePerformIO for testing purposes only. This will
-- all be pure code in the end.
runTypingWith :: TypingState -> Expr -> IO (Either ErrorList Type, TypingState)
runTypingWith state a = runStateT (runErrorT $ t a) state
  where t expr = do (type_, subs) <- typeOf expr
                    return $ normalize (apply subs type_)

runTyping :: Expr -> IO (Either ErrorList Type, TypingState)
runTyping = runTypingWith defaultTypingState

--runTypeChecker :: Typing Expr -> IO (Either ErrorList Type, TypingState)
--runTypeChecker typing = runStateT (runErrorT typing) defaultTypingState

unsafeRunTypingWith :: TypingState -> Expr -> (Either ErrorList Type, TypingState)
unsafeRunTypingWith state a = unsafePerformIO $ runStateT (runErrorT $ t a) state
  where t expr = do (type_, subs) <- typeOf expr
                    return $ normalize (apply subs type_)

unsafeRunTyping :: Expr -> (Either ErrorList Type, TypingState)
unsafeRunTyping = unsafeRunTypingWith defaultTypingState

unsafeRunUnify :: Type -> Type -> (Either ErrorList Subs, TypingState)
unsafeRunUnify type1 type2 =
  unsafePerformIO $ runStateT (runErrorT $ unify (type1, type2)) defaultTypingState

--unsafeRunTypeChecker :: Typing Expr -> (Either ErrorList Expr, TypingState)
--unsafeRunTypeChecker typing =
--  unsafePerformIO $ runStateT (runErrorT typing) defaultTypingState

typeIt :: String -> Either ErrorList Type
typeIt input = case desugarIt input of
  Left err -> Left $ ErrorList ["Parse error:\n" <> show err]
  Right block -> fst $ unsafeRunTyping block

unifyIt :: (String, String) -> Either ErrorList Subs
unifyIt (input1, input2) = do
  e1 <- grabT input1
  e2 <- grabT input2
  fst (unsafeRunUnify e1 e2)
