{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
module Language.Llama.Desugarer.Desugar where

import qualified Prelude as P
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe

import Language.Llama.Common.Common
import Language.Llama.Common.AST
import Language.Llama.Types.TypeLib hiding (log, log')
import Language.Llama.Parser.Parser

data DExpr = DExpr {_orig :: Expr, _dsrd :: AbsExpr DExpr}
           deriving (P.Show, Eq)

data DesugarerEnv = DesugarerEnv
  { dsNameSpace :: NameSpace
  , dsConstructors :: M.Map Name Type
  , dsNamesInUse :: [S.Set Name] }

type Desugar = ErrorT ErrorList (StateT DesugarerEnv IO)
type Desugarer = (Name, AbsExpr DExpr -> Bool, DExpr -> Desugar DExpr)

instance P.Show Desugarer where
  show (n, _, _) = "Desugarer '" <> unpack n <> "'"
instance Render Desugarer where
  render (n, _, _) = "Desugarer '" <> n <> "'"
instance IsExpr DExpr where unExpr = _dsrd
instance Render DExpr where render = render . bareExpr

-- | Converts an Expr to a DExpr containing the same structure.
expr2DExpr :: Expr -> DExpr
expr2DExpr e = DExpr {_orig=e, _dsrd=fmap expr2DExpr $ unExpr e}

-- | Converts a monadic AbsExpr DExpr into a DExpr.
mk :: DExpr -> Desugar (AbsExpr DExpr) -> Desugar DExpr
mk e = fmap $ \desugared -> DExpr {_orig=_orig e, _dsrd=desugared}

traverse :: Desugarer -> DExpr -> Desugar DExpr
traverse ds e | doTest ds e = doTransform ds e
              | otherwise = mk $ case unExpr e of
  Var n -> pure $ Var n
  Number n -> pure $ Number n
  String s -> pure $ String s
  Constructor c -> pure $ Constructor c
  Continue -> pure Continue
  TypeDef n t -> pure $ TypeDef n t
  Block blk -> Block <$> mapM (recNS "%b") blk
  Dot e1 e2 -> Dot <$> rec e1 <*> rec e2
  Apply e1 e2 -> Apply <$> rec e1 <*> rec e2
  Lambda arg body -> Lambda <$> recNS "%l" arg <*> recNS "%l" body
  Lambdas argsBodies -> Lambdas <$> mapM (recTupNS "%l") argsBodies
  Case expr patsBodies -> Case <$> recNS "%c" expr <*> mapM (recTupNS "%c") patsBodies
  MultiCase expr patsBodies ->
    MultiCase <$> recNS "%c" expr <*> mapM (recTupNS' "%c") patsBodies
  Tuple exprs kws -> Tuple <$> mapM rec exprs <*> pure []
  Literal (VecLiteral exprs) -> Literal . VecLiteral <$> mapM rec exprs
  Literal (SetLiteral exprs) -> Literal . SetLiteral <$> mapM rec exprs
  DeRef e1 e2 -> DeRef <$> rec e1 <*> rec e2
  Typed expr t -> rec expr >>= \e' -> return $ Typed e' t
  If c t f -> If <$> recNS "%if" c <*> recNS "%if" t <*> recMaybeNS "%if" f
  a `Then` b -> Then <$> rec a <*> rec b
  --For i c s ex -> For <$> recNS "%f" i <*> recNS "%f" c
  --                    <*> recNS "%f" s <*> recNS "%f" ex
  --ForIn e1 e2 e3 -> ForIn <$> recNS "%f" e1 <*> recNS "%f" e2 <*> recNS "%f" e3
  Define name expr -> Define name <$> recNS name expr
  PatternDef pat expr -> PatternDef <$> rec pat <*> rec expr
  Assign e1 e2 -> Assign <$> rec e1 <*> rec e2
  Return expr -> Return <$> rec expr
  Throw expr -> Throw <$> rec expr
  TryCatch expr options finally ->
    TryCatch <$> rec expr <*> mapM recTup options
                          <*> recMaybe finally
  Break expr -> Break <$> rec expr
  After e1 e2 -> After <$> rec e1 <*> rec e2
  Before e1 e2 -> Before <$> rec e1 <*> rec e2
  ObjDec od -> ObjDec <$> recOd od
  Modified m expr -> Modified m <$> rec expr
  LambdaDot e' -> LambdaDot <$> recNS "%l" e'
  Prefix name e' -> Prefix name <$> rec e'
  WildCard -> return WildCard
  InString is -> InString <$> recIs is
  Attribute expr name -> (\ex -> Attribute ex name) <$> rec expr
  GetAttrib name idx expr -> GetAttrib name idx <$> rec expr
  PatAssert pa -> PatAssert <$> recPA pa
  _ -> error $ "Expression not covered in desugarer: " <> unpack (render e)
  where doTest :: Desugarer -> DExpr -> Bool
        doTest (_, t, _) e = t $ unExpr e
        doTransform (_, _, t) e = t e
        mk = fmap $ \dsrd -> DExpr {_orig=_orig e, _dsrd=dsrd}
        mk' = mk . return
        rec = traverse ds
        recNS n expr = pushNS n *> rec expr <* popNS
        recTup (a,b) = (,) <$> rec a <*> rec b
        recTupNS n (a, b) = pushNS n *> recTup (a, b) <* popNS
        recTupNS' n (exprs, body) = do
          exprs' <- pushNS n *> mapM rec exprs
          body' <- rec body <* popNS
          return (exprs', body')
        recMaybe Nothing = return Nothing
        recMaybe (Just ex) = Just <$> rec ex
        recMaybeNS _ Nothing = return Nothing
        recMaybeNS n (Just ex) = Just <$> recNS n ex
        recIs (Interp s ex s') = Interp <$> recIs s <*> rec ex <*> recIs s'
        recIs (InterpShow s ex s') = InterpShow <$> recIs s <*> rec ex <*> recIs s'
        recIs (Bare s) = return (Bare s)
        recPA pa = case pa of
          IsLiteral e1 e2 -> IsLiteral <$> rec e1 <*> rec e2
          IsConstr name e -> IsConstr name <$> rec e
          IsTupleOf size e -> IsTupleOf size <$> rec e
          IsVectorOf size e -> IsVectorOf size <$> rec e
          And pa1 pa2 -> And <$> recPA pa1 <*> recPA pa2
        recCr cr@(ConstructorDec {constrArgs=args, constrExtends=extends}) = do
          args' <- mapM rec args
          extends' <- recMaybe extends
          return cr {constrArgs=args', constrExtends=extends'}
        recOd od@(ObjectDec {objConstrs=constrs, objAttrs=attrs}) = do
          constrs' <- mapM recCr constrs
          attrs' <- mapM rec attrs
          return $ od {objConstrs=constrs', objAttrs=attrs'}

pushNS :: Name -> Desugar ()
pushNS name = do
  modify $ \s -> s { dsNameSpace = name +: dsNameSpace s
                   , dsNamesInUse = S.singleton name : dsNamesInUse s}

popNS :: Desugar ()
popNS = modify $ \s -> s { dsNameSpace = nsTail $ dsNameSpace s
                         , dsNamesInUse = tail $ dsNamesInUse s}

{-
-- | We have a series of desugarers. Each of which has a test
-- which determines if it should run (essentially, matching on the
-- constructor type of the expression) and a transforming function
-- which takes that expression and returns its desugared version.
dsAfterBefore, dsLambdaDot, dsDot, dsLambdas, dsPrefixLine, dsForIn,
  dsPatternDef, dsInString, dsForever, dsMultiCase,
  dsIf', dsCase :: Desugarer -}
dsAfterBefore = ("Before/After", test, ds) where
  test (After _ _) = True
  test (Before _ _) = True
  test _ = False
  ds :: DExpr -> Desugar DExpr
  ds e = mk e $ case _dsrd e of
    a `After` b -> case _dsrd b of
      _b `Then` c -> do
        (a', b', c') <- (,,) <$> rec a <*> rec _b <*> rec c
        return $ b' `Then` (DExpr {_orig=_orig b, _dsrd=c' `Then` a'})
      _ -> do
        (a', b') <- (,) <$> rec a <*> rec b
        return $ b' `Then` a'
    a `Before` b -> throwError1 "before isn't done yet"
  rec = traverse ("Before/After", test, ds)
  --unusedVar = P.undefined

{-
dsLambdaDot = ("LambdaDot", test, ds) where
  test (LambdaDot _) = True
  test (Apply _ _) = True
  test _ = False
  ds e = case unExpr e of
    LambdaDot e -> do
      name <- unusedVar "_arg"
      Lambda (Var name) . Dot (Var name) <$> rec e
    Apply _ _ -> do
      name <- unusedVar "_arg"
      let (exprs, root) = unApply e
      exprs' <- mapM rec exprs
      case root of
        LambdaDot expr -> do
          expr' <- rec expr
          let body = foldl Apply (Dot (Var name) expr') exprs'
          return $ Lambda (Var name) body
        expr -> foldl Apply <$> rec expr <*> pure exprs'
  rec = traverse ("LambdaDot", test, ds)


dsPatternDef = ("Patterned definition", test, ds) where
  rec = traverse ("Patterned definition", test, ds)
  test (PatternDef _ _) = True
  test _ = False
  ds :: Expr -> Desugar Expr
  ds (PatternDef pat expr) = go (unApply pat) where
    go :: ([Expr], Expr) -> Desugar Expr
    go (exprs, Var name) = do
      body :: Expr <- rec expr
      return $ Define name (P.foldr Lambda body exprs)
    go (_, pat') = rec expr >>= assertsAndDefs pat' >>= \case
      (Nothing, defs) -> return defs
      (Just b, defs) -> return $ If (PatAssert b) defs patternMatchError
-}
dsDot = ("Dot", test, ds) where
  test (Dot _ _) = True
  test _ = False
  ds e = mk e $ case unExpr e of
    Dot a b -> Apply <$> rec b <*> rec a
  rec = traverse ("Dot", test, ds)


{-dsLambdas = ("Lambdas", test, ds) where
  test (Lambdas _) = True
  test _ = False
  ds e = mk e $ case unExpr e of
    Lambdas argsBodies -> do
      var <- DExpr (_orig e) . Var <$> unusedVar "_arg"

      Lambda var <$> rec (DExpr (_orig e) (Case var argsBodies))
  rec = traverse ("Lambdas", test, ds)
-}
{-
dsPrefixLine = ("Prefix line", test, ds) where
  test (Block _) = True
  test _ = False
  ds (Block blk) = Block <$> first blk
  first (Prefix _ _ : _) =
    throwError1 "The first line in a block may not start with a prefix"
  first (e:rest) = go (e:rest)
  go [] = return []
  go (e:Prefix op e':rest) = do
    newE <- rec e
    newE' <- rec e'
    go (binary op newE newE' : rest)
  go (e:rest) = doRest e rest
  doRest e rest = (:) <$> rec e <*> go rest
  rec = traverse ("Prefix line", test, ds)

dsForIn = ("For in", test, ds) where
  test (ForIn _ _ _) = True
  test _ = False
  ds (ForIn (Var name) cont expr) = do
    iterName <- unusedVar "_iter"
    let iter = Var iterName
    -- _iter = mut cont.iter
    let init = Define iterName $ Modified Mut (Dot cont (Var "iter"))
    -- name = _iter.get
    let stmt = Define name (Dot iter (Var "get"))
    -- _iter.valid?
    let cond = Dot iter (Var "valid?")
    -- _iter.forward!
    let step = Dot iter (Var "forward!")
    -- append
    let blk = case expr of Block b -> Block $ stmt:b
                           _ -> Block $ [stmt, expr]
    return $ For init cond step blk

dsForever = ("Forever", test, ds) where
  test (Forever _) = True
  test _ = False
  ds (Forever expr) = For unit true unit <$> rec expr
  rec = traverse ("Forever", test, ds)
-}

dsInString = ("Interpolated strings", test, ds) where
  test (InString _) = True
  test _ = False
  ds e = case unExpr e of
    (InString is) -> go is
    where
      go :: InString DExpr -> Desugar DExpr
      go (Bare s) = return $ DExpr (_orig e) $ String s
      go (Interp is1 e is2) = bin <$> go is1 <*> rec e <*> go is2
      go (InterpShow is e is') = bin <$> go is <*> e' <*> go is' where
        e' = do
          let shw = DExpr (_orig e) (Var "show")
          DExpr (_orig e) . Apply shw <$> rec e
  bin :: DExpr -> DExpr -> DExpr -> DExpr
  bin left e right = P.foldr addIfNotEmpty right [left, e]
  addIfNotEmpty :: DExpr -> DExpr -> DExpr
  addIfNotEmpty e1 e2 = case (unExpr e1, unExpr e2) of
    (String "", _) -> e2
    (_, String "") -> e1
    _ -> DExpr (_orig e1) $ Binary "<>" e1 e2
  rec = traverse ("Interpolated strings", test, ds)

dsMultiCase = tup where
  tup = ("Multicase", test, ds)
  test (MultiCase _ _) = True
  test _ = False
  ds e = mk e $ case unExpr e of
    MultiCase expr patsArgs -> Case <$> rec expr <*> go [] patsArgs
  go alts [] = return $ P.reverse alts
  go alts (([], _):rest) = go alts rest
  go alts ((e:exprs, res):rest) = do
    e' <- rec e
    res' <- rec res
    go ((e', res'):alts) ((exprs, res'):rest)
  rec = traverse tup

dsCase = tup where
  tup = ("Case", test, ds)
  test (Case _ _) = True
  test _ = False
  rec = traverse tup
  ds :: DExpr -> Desugar DExpr
  ds e = mk e $ case unExpr e of
    Case e options -> do
      e' <- rec e
      options' <- mapM (\(a,b) -> (,) <$> rec a <*> rec b) options
      go e' options'
  go :: DExpr -> [(DExpr, DExpr)] -> Desugar (AbsExpr DExpr)
  go e = \case
    [] -> return $ patternMatchError e
    (pat, res):opts -> assertsAndDefs pat e >>= \case
      (Nothing, Nothing) -> return $ unExpr res
      (Nothing, Just defs) -> return $ defs `Then` res
      (Just cond, Nothing) -> do
        absdexpr <- go e opts
        let dexpr = DExpr (_orig pat) absdexpr
        let pa = DExpr (_orig pat) $ PatAssert cond
        let body = DExpr (_orig pat) (unExpr res)
        return $ If pa body (Just dexpr)
      (Just cond, Just defs) -> do
        absdexpr <- go e opts
        let dexpr = DExpr (_orig pat) absdexpr
        let pa = DExpr (_orig pat) $ PatAssert cond
        let body = DExpr (_orig pat) $ defs `Then` res
        return $ If pa body (Just dexpr)
  patternMatchError :: DExpr -> AbsExpr DExpr
  patternMatchError e = do
    let d = DExpr (_orig e)
        unit = d $ Tuple [] []
    Throw $ d (Apply (d $ Constructor "PatternMatchError") unit)

assertsAndDefs :: DExpr
               -> DExpr
               -> Desugar (Maybe (PatAssert DExpr), Maybe DExpr)
assertsAndDefs start expr = fst <$> runStateT (go start expr) ("", 0) where
  unit = DExpr (_orig start) $ Tuple [] []
  go :: Expr -> Expr
     -> StateT (Name, Int) Desugar (Maybe (PatAssert DExpr), DExpr)
  go expr matchWith = case expr of
    Number n -> return (lit $ Number n, unit)
    String s -> return (lit $ String s, unit)
    Var v -> case matchWith of
      Var v' | v' == v -> return (Nothing, unit)
      _ -> return (Nothing, Define v matchWith)
    Constructor name -> do
      modify (\(_, i) -> (name, i))
      return (Just $ name `IsConstr` matchWith, unit)
    Tuple exprs _ -> do
      list <- mapM convertWithIndex $ P.zip [0..] exprs
      return (mkBool (P.length exprs) list, mkAssigns list)
      where convertWithIndex (i, e) = aRef i >>= go e
    Apply a b -> do
      (bool1, assn1) <- go a matchWith
      (_, i)   <- get
      (bool2, assn2) <- aRef i >>= go b
      modify (\(name, i) -> (name, i + 1))
      return (bool1 `and` bool2, assn1 <> assn2)
    _ -> lift $ throwErrorC ["Unhandled case: ", render expr]
    where lit expr = Just $ matchWith `IsLiteral` expr
          aRef n = do
            (name, _) <- get
            return $ GetAttrib name n matchWith
          Just pa1 `and` Just pa2 = Just $ pa1 `And` pa2
          a `and` b = a `mplus` b
          -- make an AND of all the a==b expressions in subcompilations
          mkBool len list =
            P.foldl and (Just $ IsTupleOf len matchWith) (fst <$> list)
          -- concatenate all of the assignments from the subcompilations
          mkAssigns list = mconcat (P.snd <$> list)

{-
-- | @unApply@ "unwinds" a series of applies into a list of
-- expressions that are on the right side, paired with the
-- root-level expression. So for example `foo bar baz` which
-- is `Apply (Apply foo bar) baz` would become `([baz, bar], foo)`.
unApply :: Expr -> ([Expr], Expr)
unApply expr = go [] expr where
  go exprs (Apply a b) = go (b:exprs) a
  go exprs root = (exprs, root)



-- | TODO: we should probably be able to search *forwards* in
-- the AST, as well as just looking at current names. This isn't as
-- efficient but it might be negligible for the time being.
addUsedName :: Name -> Desugar Name
addUsedName name = do
  top:rest <- dsNamesInUse <$> get >>= \case
    [] -> return (mempty:[])
    tr -> return tr
  modify $ \s -> s {dsNamesInUse = S.insert name top : rest}
  return name

unusedVar :: Name -> Desugar Name
unusedVar start = loop Nothing where
  toName Nothing = start
  toName (Just num) = start <> render num
  loop :: Maybe Int -> Desugar Name
  loop n = let name = toName n in
    isNameInUse (toName n) >>= \case
      True -> loop . Just $ case n of
        Nothing -> 0
        Just n' -> n' + 1
      False -> addUsedName name

isNameInUse :: Name -> Desugar Bool
isNameInUse name = do
  names <- get <!> dsNamesInUse
  loop names
  where loop [] = return False
        loop (names:ns) = case S.member name names of
          True -> return True
          False -> loop ns

defaultEnv :: DesugarerEnv
defaultEnv = DesugarerEnv {
    dsNameSpace = mempty
  , dsConstructors = mempty
  , dsNamesInUse = mempty
  }

log xs = liftIO $ P.putStrLn $ P.concat $ fmap unpack xs

desugarers :: [Desugarer]
desugarers = [ dsAfterBefore, dsInString, dsMultiCase]
             --, dsLambdaDot
             --, dsPrefixLine
             --, dsLambdas
             --, dsDot
             --, dsForIn
             --, dsForever
             --, dsPatternDef
             --, dsInString
             --, dsMultiCase
             --, dsIf'
             --, dsCase]

desugar :: Expr -> Desugar DExpr
desugar = go desugarers . expr2DExpr where
  go [] e = return e
  go (d:ds) e = traverse d e >>= go ds

runDesugarWith :: DesugarerEnv
               -> Expr
               -> (Either ErrorList DExpr, DesugarerEnv)
runDesugarWith state e =
  unsafePerformIO $ runStateT (runErrorT (desugar e)) state

runDesugarWith' :: Desugarer
                -> DesugarerEnv
                -> Expr
                -> (Either ErrorList DExpr, DesugarerEnv)
runDesugarWith' ds state e = do
  let de = expr2DExpr e
  unsafePerformIO $ runStateT (runErrorT (traverse ds de)) state

runDesugar :: Expr -> Either ErrorList DExpr
runDesugar = fst . runDesugarWith defaultEnv

runDesugar' :: Desugarer -> Expr -> Either ErrorList DExpr
runDesugar' ds expr = fst $ runDesugarWith' ds defaultEnv expr

desugarIt :: String -> Either ErrorList DExpr
desugarIt input = case parse input of
  Left err -> Left $ ErrorList [show err]
  Right expr -> runDesugar expr

testString :: String -> IO ()
testString input = case desugarIt input of
  Left err -> error $ P.show err
  Right dexpr -> putStrLn $ unpack $ render dexpr

--desugarIt' :: (Desugarer, String) -> Either ErrorList DExpr
--desugarIt' (ds, input) = case grab input of
--  Left err -> error $ P.show err
--  Right expr -> runDesugar' ds expr

-}
