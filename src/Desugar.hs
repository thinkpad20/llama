{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Desugar where

import Prelude (IO, Show(..), Eq(..), Ord(..), Bool(..),
                Double, String, Maybe(..), Int, Monad(..),
                ($), (.), floor, map, Functor(..), mapM,
                (+), (-), elem, Either(..), tail, otherwise,
                error, undefined, fst, putStrLn, (||), foldl)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Prelude as P
import System.IO.Unsafe

import Common
import AST
import TypeLib hiding (log, log')
import Parser
import Data.Text hiding (tail, foldl, partition, init)

type Desugarer a = (Name, a -> Bool, a -> Desugar a)

instance Show (Desugarer a) where
  show (n, _, _) = "Desugarer '" <> unpack n <> "'"
instance Render (Desugarer a) where
  render (n, _, _) = "Desugarer '" <> n <> "'"

data DesugarerEnv = DesugarerEnv {
    dsNameSpace :: NameSpace
  , dsConstructors :: M.Map Name Type
  , dsNamesInUse :: [S.Set Name]
  , dsObjDecs :: M.Map Name ObjectDec
  }

type Desugar = ErrorT ErrorList (StateT DesugarerEnv IO)

doTest :: Desugarer Expr -> Expr -> Bool
doTest (_, t, _) e = t e

doTransform :: Desugarer Expr -> Expr -> Desugar Expr
doTransform (_, _, t) e = t e

traverse :: Desugarer Expr -> Expr -> Desugar Expr
traverse ds e | doTest ds e = doTransform ds e
              | otherwise = case e of
  Var _ -> return e
  Number _ -> return e
  String _ -> return e
  Constructor _ -> return e
  Continue -> return e
  TypeDef _ _ -> return e
  Block blk -> Block <$> mapM (recNS "%b") blk
  Dot e1 e2 -> Dot <$> rec e1 <*> rec e2
  Apply e1 e2 -> Apply <$> rec e1 <*> rec e2
  Lambda arg body -> Lambda <$> recNS "%l" arg <*> recNS "%l" body
  Lambdas argsBodies -> Lambdas <$> mapM (recTupNS "%l") argsBodies
  Case expr patsBodies -> Case <$> recNS "%c" expr <*> mapM (recTupNS "%c") patsBodies
  Tuple exprs kws -> Tuple <$> mapM rec exprs <*> mapM recKw kws
  Literal (ArrayLiteral exprs) -> Literal . ArrayLiteral <$> mapM rec exprs
  Literal (ListLiteral exprs) -> Literal . ListLiteral <$> mapM rec exprs
  Literal (SetLiteral exprs) -> Literal . SetLiteral <$> mapM rec exprs
  DeRef e1 e2 -> DeRef <$> rec e1 <*> rec e2
  Typed expr t -> rec expr >>= \e' -> return $ Typed e' t
  If c t f -> If <$> recNS "%if" c <*> recNS "%if" t <*> recNS "%if" f
  If' c t -> If' <$> recNS "%if" c <*> recNS "%if" t
  For i c s ex -> For <$> recNS "%f" i <*> recNS "%f" c
                      <*> recNS "%f" s <*> recNS "%f" ex
  ForIn e1 e2 e3 -> ForIn <$> recNS "%f" e1 <*> recNS "%f" e2 <*> recNS "%f" e3
  Define name expr -> Define name <$> recNS name expr
  Extend name expr -> Extend name <$> recNS name expr
  PatternDef pat expr -> PatternDef <$> rec pat <*> rec expr
  Assign e1 e2 -> Assign <$> rec e1 <*> rec e2
  Return expr -> Return <$> rec expr
  Throw expr -> Throw <$> rec expr
  Break expr -> Break <$> rec expr
  After e1 e2 -> After <$> rec e1 <*> rec e2
  Before e1 e2 -> Before <$> rec e1 <*> rec e2
  ObjDec od -> ObjDec <$> recOd od
  Modified m expr -> Modified m <$> rec expr
  LambdaDot e' -> LambdaDot <$> recNS "%l" e'
  Prefix name e' -> Prefix name <$> rec e'
  WildCard -> return WildCard
  _ -> error $ "Expression not covered in desugarer: " <> P.show e
  where rec = traverse ds
        recNS n expr = pushNS n *> rec expr <* popNS
        recTup (a,b) = (,) <$> rec a <*> rec b
        recTupNS n (a, b) = pushNS n *> recTup (a, b) <* popNS
        recKw = undefined
        recMaybe Nothing = return Nothing
        recMaybe (Just ex) = Just <$> rec ex
        recCr cr@(ConstructorDec { constrArgs=args, constrExtends=extends
                                 , constrLogic=logic }) = do
          args' <- mapM rec args
          extends' <- recMaybe extends
          logic' <- recMaybe logic
          return cr {constrArgs=args', constrExtends=extends', constrLogic=logic'}
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
dsAfterBefore, dsLambdaDot, dsDot, dsLambdas, dsPrefixLine, dsForIn,
  dsPatternDef :: Desugarer Expr
dsAfterBefore = ("Before/After", test, ds) where
  test (After _ _) = True
  test (Before _ _) = True
  test _ = False
  ds (e `After` Block es) = do
    es' <- mapM rec es
    e' <- rec e
    return $ Block $ es' <> [e']
  ds (e1 `After` e2) = do
    e1' <- rec e1
    e2' <- rec e2
    return $ Block [e2', e1']
  ds (e `Before` Block es) = do
    var <- unusedVar "_var"
    e' <- rec e
    es' <- mapM rec es
    return $ Block $ Define var e':es' <> [Var var]
  ds (e1 `Before` e2) = do
    var <- unusedVar "_var"
    e1' <- rec e1
    e2' <- rec e2
    return $ Block [Define var e1', e2', Var var]
  rec = traverse ("Before/After", test, ds)

dsLambdaDot = ("LambdaDot", test, ds) where
  test (LambdaDot _) = True
  test (Apply _ _) = True
  test _ = False
  ds (LambdaDot e) = do
    name <- unusedVar "_arg"
    Lambda (Var name) . Dot (Var name) <$> rec e
  ds e@(Apply _ _) = do
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
    go (_, _) = throwErrorC ["Invalid pattern: ", render expr]

dsDot = ("Dot", test, ds) where
  test (Dot _ _) = True
  test _ = False
  ds (Dot a b) = Apply <$> rec b <*> rec a
  rec = traverse ("Dot", test, ds)

dsLambdas = ("Lambdas", test, ds) where
  test (Lambdas _) = True
  test _ = False
  ds (Lambdas argsBodies) = do
    var <- Var <$> unusedVar "_arg"
    Lambda var <$> rec (Case var argsBodies)
  rec = traverse ("Lambdas", test, ds)

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

{-

for foo in bar { baz; qux }

for mut _iter = bar.iter; _iter.valid; _iter.forward!
  foo = _iter.get
  baz;
  qux;

-}

-- | @unApply@ "unwinds" a series of applies into a list of
-- expressions that are on the right side, paired with the
-- root-level expression. So for example `foo bar baz` which
-- is `Apply (Apply foo bar) baz` would become `([baz, bar], foo)`.
unApply :: Expr -> ([Expr], Expr)
unApply expr = go [] expr where
  go exprs (Apply a b) = go (b:exprs) a
  go exprs root = (exprs, root)

-- | TODO: we should probably be able to retroactively change a name;
-- for example if we introduce the name `_arg` and then a *later* scope
-- introduces that name, we should be able to go back and remap `_arg`.
-- Alternatively, we could change `isNameInUse` to search *forwards* in
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
  , dsObjDecs = mempty
  }

desugarers :: [Desugarer Expr]
desugarers = [ dsAfterBefore
             , dsLambdaDot
             , dsPrefixLine
             , dsLambdas
             , dsDot
             , dsForIn
             , dsPatternDef]

desugar :: Expr -> Desugar Expr
desugar expr = go desugarers expr where
  go [] e = return e
  go (d:ds) e = traverse d e >>= go ds

log :: Text -> Desugar ()
log = lift . lift . putStrLn . unpack
log' :: [Text] -> Desugar ()
log' = log . mconcat

runDesugarWith :: DesugarerEnv -> Expr -> (Either ErrorList Expr, DesugarerEnv)
runDesugarWith state e =
  unsafePerformIO $ runStateT (runErrorT (desugar e)) state

runDesugarWith' :: Desugarer Expr -> DesugarerEnv -> Expr -> (Either ErrorList Expr, DesugarerEnv)
runDesugarWith' ds state e =
  unsafePerformIO $ runStateT (runErrorT (traverse ds e)) state

runDesugar :: Expr -> Either ErrorList Expr
runDesugar = fst . runDesugarWith defaultEnv

runDesugar' :: Desugarer Expr -> Expr -> Either ErrorList Expr
runDesugar' ds expr = fst $ runDesugarWith' ds defaultEnv expr

desugarIt :: String -> Either ErrorList Expr
desugarIt input = case grab input of
  Left err -> error $ P.show err
  Right exprs -> runDesugar (Block exprs) >>= \case
    Block [Block exprs'] -> return $ Block exprs'
    result -> return result

desugarIt' :: (Desugarer Expr, String) -> Either ErrorList Expr
desugarIt' (ds, input) = case grab input of
  Left err -> error $ P.show err
  Right exprs -> runDesugar' ds (Block exprs) >>= \case
    Block [Block exprs'] -> return $ Block exprs'
    result -> return result
