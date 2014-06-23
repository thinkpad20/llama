{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Llama.Desugarer.Desugar (desugarIt, DExpr) where

import qualified Prelude as P
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO.Unsafe

import Language.Llama.Common.Common
import Language.Llama.Common.AST
--import Language.Llama.Types.TypeLib hiding (log, log')
import Language.Llama.Parser.Parser

data DExpr = DExpr {_orig :: Expr, _dsrd :: AbsExpr DExpr}
           deriving (P.Show, Eq)

newtype NameSpace = NameSpace [Name] deriving P.Show

instance Monoid NameSpace where
  mempty = NameSpace mempty
  (NameSpace ns) `mappend` (NameSpace ns') = NameSpace (ns <> ns')

(+:) :: Name -> NameSpace -> NameSpace
name +: (NameSpace ns) = NameSpace (name : ns)

nsTail :: NameSpace -> NameSpace
nsTail (NameSpace ns) = NameSpace (tail ns)

data DesugarerEnv = DesugarerEnv
  { dsNameSpace :: NameSpace
  , dsConstructors :: M.Map Name ()
  , dsNamesInUse :: [S.Set Name] }

type Desugar = ErrorT ErrorList (StateT DesugarerEnv IO)
type Desugarer = (Name, AbsExpr DExpr -> Bool, DExpr -> Desugar DExpr)

instance P.Show Desugarer where
  show (n, _, _) = "Desugarer '" <> unpack n <> "'"
instance Render Desugarer where
  render (n, _, _) = "Desugarer '" <> n <> "'"
instance IsExpr DExpr where unExpr = _dsrd
instance Sourced DExpr where source = source . _orig
instance Render DExpr where render = rndr True . bareExpr

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
  Float n -> pure $ Float n
  Int n -> pure $ Int n
  String s -> pure $ String s
  Constructor c -> pure $ Constructor c
  Continue -> pure Continue
  TypeDef n t -> pure $ TypeDef n t
  Dot e1 e2 -> Dot <$> rec e1 <*> rec e2
  Apply e1 e2 -> Apply <$> rec e1 <*> rec e2
  Binary op e1 e2 -> Binary op <$> rec e1 <*> rec e2
  Prefix op e -> Prefix op <$> rec e
  Postfix e op -> flip Postfix op <$> rec e
  Lambda arg body -> Lambda arg <$> recNS "%l" body
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
  Define name expr -> Define name <$> recNS name expr
  PatternDef pat expr -> PatternDef <$> rec pat <*> rec expr
  Return expr -> Return <$> rec expr
  Throw expr -> Throw <$> rec expr
  TryCatch expr options finally ->
    TryCatch <$> rec expr <*> mapM recTup options <*> recMaybe finally
  Break expr -> Break <$> rec expr
  After e1 e2 -> After <$> rec e1 <*> rec e2
  Before e1 e2 -> Before <$> rec e1 <*> rec e2
  ObjDec od -> ObjDec <$> recOd od
  Modified m expr -> Modified m <$> rec expr
  LambdaDot e rest -> LambdaDot <$> recNS "%l" e <*> mapM (recNS "%l") rest
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

-- | We have a series of desugarers. Each of which has a test
-- which determines if it should run (essentially, matching on the
-- constructor type of the expression) and a transforming function
-- which takes that expression and returns its desugared version.
dsAfterBefore :: Desugarer
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

dsLambdaDot :: Desugarer
dsLambdaDot = ("LambdaDot", test, ds) where
  test (LambdaDot _ _) = True
  test _ = False
  ds e = let mk' ex = DExpr (_orig e) ex in case unExpr e of
    LambdaDot e rest -> do
      name <- unusedVar "_arg"
      e' <- rec e
      let pat = mk' $ Var name
      return $ mk' $ Lambda name $ mk' $ Dot pat e'
  rec = traverse ("LambdaDot", test, ds)

dsPatternDef :: Desugarer
dsPatternDef = ("Patterned definition", test, ds) where
  rec = traverse ("Patterned definition", test, ds)
  test (PatternDef _ _) = True
  test _ = False
  ds :: DExpr -> Desugar DExpr
  ds d = case unExpr d of
    PatternDef pat expr -> go (unApply pat) where
      go :: ([DExpr], DExpr) -> Desugar DExpr
      go (exprs, root) = case unExpr root of
        Var name -> do
          body :: DExpr <- rec expr
          let lambda e1 e2 = DExpr (_orig d) $ Lambdas [(e1, e2)]
              body' = foldr lambda body exprs
          return $ DExpr (_orig root) $ Define name body'
        pat' -> rec expr >>= assertsAndDefs root >>= \case
          (_, Nothing) -> throwErrorC ["Invalid pattern: ", render pat]
          (Nothing, Just defs) -> return defs
          (Just b, Just defs) -> return $ mk' $ If cond defs false where
            mk' e = DExpr (_orig root) e
            cond = mk' $ PatAssert b
            false = Just $ mk' $ patternMatchError root

dsDot :: Desugarer
dsDot = ("Dot", test, ds) where
  test (Dot _ _) = True
  test _ = False
  ds e = mk e $ case unExpr e of
    Dot a b -> Apply <$> rec b <*> rec a
  rec = traverse ("Dot", test, ds)

dsLambdas :: Desugarer
dsLambdas = ("Lambdas", test, ds) where
  test (Lambdas _) = True
  test _ = False
  ds e = mk e $ case unExpr e of
    Lambdas [(unExpr -> Var name, res)] -> Lambda name <$> rec res
    Lambdas argsBodies -> do
      name <- unusedVar "_arg"
      let var = DExpr (_orig e) $ Var name
      Lambda name <$> rec (DExpr (_orig e) (Case var argsBodies))
  rec = traverse ("Lambdas", test, ds)

dsInString :: Desugarer
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

-- | Desugars binary and unary operators into variable applications
dsBinary :: Desugarer
dsBinary = tup where
  tup = ("Prefix", test, ds)
  test (Prefix _ _) = True
  test (Postfix _ _) = True
  test (Binary _ _ _) = True
  test _ = False
  rec = traverse tup
  ds e = mk e $ case unExpr e of
    Prefix op expr ->
      Apply (DExpr (_orig e) $ Var $ op <> "_") <$> rec expr
    Postfix expr op ->
      Apply (DExpr (_orig e) $ Var $ "_" <> op) <$> rec expr
    Binary op a b -> do
      let var = DExpr (_orig e) $ Var $ "_" <> op <> "_"
      inner <- DExpr (_orig e) . Apply var <$> rec a
      Apply inner <$> rec b

-- | Translates symbol variables into their "readable" forms with @toString@.
dsSymbols :: Desugarer
dsSymbols = tup where
  tup = ("Prefix", test, ds)
  test (Var n) | isSymbol n = True
  test (Constructor n) | isSymbol n = True
  test _ = False
  rec = traverse tup
  ds e = mk e $ case unExpr e of
    Var n | T.head n == '_' -> return $ Var $ toString "post" n
          | T.last n == '_' -> return $ Var $ toString "pre" n
          | otherwise -> return $ Var $ toString "" n
    Constructor n -> return $ Constructor $ toString "" n

-- | Desugars a MultiCase (with one or more expressions per alternative) into
-- a Case (with only one expression per alternative).
dsMultiCase :: Desugarer
dsMultiCase = tup where
  tup = ("MultiCase", test, ds)
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

-- | Desugars a Case into a series of if/else statements and assignments. If
-- the last expression in the series is not a variable or wildcard, the last
-- else will be to throw a @PatternMatchError@.
dsCase :: Desugarer
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
      name <- unusedVar "_ref"
      let var = DExpr (_orig e) $ Var name
          def = DExpr (_orig e) $ Define name e'
      Then def . DExpr (_orig e) <$> go var options'
  go :: DExpr -> [(DExpr, DExpr)] -> Desugar (AbsExpr DExpr)
  go e = \case
    [] -> return $ patternMatchError e
    (pat, res):opts -> assertsAndDefs pat e >>= \case
      (Nothing, Nothing) -> return $ unExpr res
      (Nothing, Just defs) -> return $ defs `Then` res
      (Just cond, defs) -> do
        let pa = DExpr (_orig pat) $ PatAssert cond
            body = case defs of
                    Just defs -> DExpr (_orig pat) $ defs `Then` res
                    Nothing -> res
        If pa body . Just . DExpr (_orig pat) <$> go e opts

patternMatchError :: DExpr -> AbsExpr DExpr
patternMatchError e = do
  let d = DExpr (_orig e)
      unit = d $ Tuple [] []
  Throw $ d (Apply (d $ Constructor "PatternMatchError") unit)

-- | Takes a pattern and a result, and converts it into a pattern assertion
-- and a set of definitions to make. The assertions match the pattern, and
-- the definitions make assignments of any free variables.
assertsAndDefs :: DExpr
               -> DExpr
               -> Desugar (Maybe (PatAssert DExpr), Maybe DExpr)
assertsAndDefs start expr = fst <$> runStateT (go start expr) ("", 0) where
  go :: DExpr -> DExpr
     -> StateT (Name, Int) Desugar (Maybe (PatAssert DExpr), Maybe DExpr)
  go expr matchWith = case unExpr expr of
    Int n -> return (lit expr, Nothing)
    Float n -> return (lit expr, Nothing)
    String s -> return (lit expr, Nothing)
    Var v -> return (Nothing, Just $ DExpr (_orig expr) $ Define v matchWith)
    Constructor name -> do
      modify (\(_, i) -> (name, i))
      return (Just $ name `IsConstr` matchWith, Nothing)
    Tuple exprs _ -> do
      list <- mapM convertWithIndex $ P.zip [0..] exprs
      return (mkBool (P.length exprs) list, mkAssigns list)
      where convertWithIndex (i, e) = go e =<< DExpr (_orig expr) <$> aRef i
    Apply a b -> do
      (bool1, assn1) <- go a matchWith
      (_, i)         <- get
      (bool2, assn2) <- DExpr (_orig expr) <$> aRef i >>= go b
      modify (\(name, i) -> (name, i + 1))
      return (bool1 `and` bool2, assn1 `combine` assn2)
    _ -> lift $ throwErrorC ["Unhandled case: ", render expr]
    where lit expr = Just $ matchWith `IsLiteral` expr
          combine Nothing d = d
          combine d Nothing = d
          combine (Just d1) (Just d2) = Just $ DExpr (_orig d1) (d1 `Then` d2)
          aRef n = do
            (name, _) <- get
            return $ GetAttrib name n matchWith
          and Nothing x = x
          and x Nothing = x
          and (Just x) (Just y) = Just (x `And` y)
          -- make an AND of all the a==b expressions in subcompilations
          mkBool len list =
            P.foldl and (Just $ IsTupleOf len matchWith) (fst <$> list)
          -- concatenate all of the assignments from the subcompilations
          mkAssigns list = foldl combine Nothing (P.snd <$> list)

toString :: Text -> Text -> Text
toString suffix sym = res <> suffix' where
  suffix' = case suffix of
    "" -> ""
    _ -> "$" <> suffix
  res = case sym of
    { ">" -> "$gt"; "<" -> "$lt"; "==" -> "$eq"; ">=" -> "$geq"; "<=" -> "$leq"
    ; "~" -> "$neg"; "+" -> "$add"; "-" -> "$sub"; "*" -> "$mult"
    ; "/" -> "$div"; "&&" -> "$and"; "||" -> "$or"; "^" -> "$pow"
    ; "::" -> "$cons"; "<>" -> "$append"; "!" -> "$access"; "$" -> "$applyl"
    ; "|>" -> "$applyr"; "~>" -> "$composer"; "<~" -> "$composel"
    ; s -> unpack s ! map fromChar ! mconcat ! pack }
  fromChar = \case
    { '>' -> "$gt"; '<' -> "$lt"; '=' -> "$eq"; '~' -> "$tilde"
    ; '+' -> "$plus"; '-' -> "$minus"; '*' -> "$star"; '/' -> "$fslash"
    ; '\\' -> "$bslash"; '&' -> "$amp"; '|' -> "$pipe"; '!' -> "$bang"
    ; '@' -> "$at"; ':' -> "$colon"}

-- | @unApply@ "unwinds" a series of applies into a list of
-- expressions that are on the right side, paired with the
-- root-level expression. So for example `foo bar baz` which
-- is `Apply (Apply foo bar) baz` would become `([baz, bar], foo)`.
unApply :: DExpr -> ([DExpr], DExpr)
unApply expr = go [] expr where
  go exprs e = case unExpr e of
    Apply a b -> go (b:exprs) a
    _         -> (exprs, e)

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

-- | Creates an unused variable name beginning with @start@.
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

-- | Sees if a name is in use.
isNameInUse :: Name -> Desugar Bool
isNameInUse name = do
  names <- get <!> dsNamesInUse
  loop names
  where loop [] = return False
        loop (names:ns) = case S.member name names of
          True -> return True
          False -> loop ns

defaultEnv :: DesugarerEnv
defaultEnv = DesugarerEnv
  { dsNameSpace    = mempty
  , dsConstructors = mempty
  , dsNamesInUse   = mempty }

log :: [Text] -> Desugar ()
log xs = liftIO $ P.putStrLn $ P.concat $ fmap unpack xs

desugarers :: [Desugarer]
desugarers = [ dsAfterBefore
             --, dsLambdaDot
             --, dsPrefixLine
             , dsDot
             --, dsForIn
             --, dsForever
             , dsInString
             , dsBinary
             --, dsSymbols
             , dsPatternDef
             , dsLambdas
             , dsMultiCase
             , dsCase]

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

test :: String -> IO ()
test input = case desugarIt input of
  Left err -> error $ P.show err
  Right dexpr -> putStrLn $ unpack $ render dexpr

--desugarIt' :: (Desugarer, String) -> Either ErrorList DExpr
--desugarIt' (ds, input) = case grab input of
--  Left err -> error $ P.show err
--  Right expr -> runDesugar' ds expr
