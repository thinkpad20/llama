{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Desugar where

import Prelude hiding (log)
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe

import Common
import AST
import TypeLib hiding (log, log')
import Parser

type Desugarer a = (a -> Bool, a -> Desugar a)

data DesugarerEnv = DesugarerEnv {
    dsNameSpace :: [Name]
  , dsConstructors :: M.Map Name Type
  , dsNamesInUse :: [S.Set Name]
  }

type Desugar = ErrorT ErrorList (StateT DesugarerEnv IO)

test :: Desugarer Expr -> Expr -> Bool
test (t, _) e = t e

transform :: Desugarer Expr -> Expr -> Desugar Expr
transform (_, t) e = t e

traverse :: Desugarer Expr -> Expr -> Desugar Expr
traverse ds e | test ds e = transform ds e
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
  While c b -> While <$> recNS "%w" c <*> recNS "%w" b
  For e1 e2 e3 -> For <$> recNS "%f" e1 <*> recNS "%f" e2 <*> recNS "%f" e3
  Define name expr -> Define name <$> recNS name expr
  Extend name expr -> Extend name <$> recNS name expr
  Assign e1 e2 -> Assign <$> rec e1 <*> rec e2
  Return expr -> Return <$> rec expr
  Throw expr -> Throw <$> rec expr
  Break expr -> Break <$> rec expr
  After e1 e2 -> After <$> rec e1 <*> rec e2
  Before e1 e2 -> Before <$> rec e1 <*> rec e2
  --ObjDec ObjectDec
  Modified m expr -> Modified m <$> rec expr
  LambdaDot e' -> LambdaDot <$> recNS "%l" e'
  Prefix name e' -> Prefix name <$> rec e'
  _ -> error $ "Expression not covered in desugarer: " <> show e
  where rec = traverse ds
        recNS n e = pushNS n *> rec e <* popNS
        recTup (a,b) = (,) <$> rec a <*> rec b
        recTupNS n (a, b) = pushNS n *> recTup (a, b) <* popNS
        recKw = undefined

{-
TODO: we're repeating steps unnecessarily here. We should really treat it more
as a pipes-and-filters model, with each desugarer being applied once over the
entire AST, rather than testing at each step. Or really, what we need to do is
sit down and take a critical look at what's the most efficient way to do this!
-}

pushNS :: Name -> Desugar ()
pushNS name = do
  modify $ \s -> s { dsNameSpace = name : dsNameSpace s
                   , dsNamesInUse = S.singleton name : dsNamesInUse s}
popNS = modify $ \s -> s { dsNameSpace = tail $ dsNameSpace s
                         , dsNamesInUse = tail $ dsNamesInUse s}

-- | We have a series of desugarers. Each of which has a test
-- which determines if it should run (essentially, matching on the
-- constructor type of the expression) and a transforming function
-- which takes that expression and returns its desugared version.
dsAfter, dsLambdaDot, dsDot, dsLambdas, dsPrefixLine :: Desugarer Expr
dsAfter = (test, ds) where
  test (After _ _) = True
  test _ = False
  ds (e `After` Block es) = do
    es' <- mapM rec es
    e' <- rec e
    return $ Block $ es' ++ [e']
  ds (e1 `After` e2) = do
    e1' <- rec e1
    e2' <- rec e2
    return $ Block [e2', e1']
  rec = traverse (test, ds)

dsLambdaDot = (test, ds) where
  test (LambdaDot _) = True
  test _ = False
  ds (LambdaDot e) = do
    name <- unusedVar
    Lambda (Var name) <$> rec e
  rec = traverse (test, ds)


dsDot = (test, ds) where
  test (Dot _ _) = True
  test _ = False
  ds (Dot a b) = return (Apply b a)

dsLambdas = (test, ds) where
  test (Lambdas _) = True
  test _ = False
  ds (Lambdas argsBodies) = do
    name <- unusedVar
    Lambda (Var name) <$> rec (Case (Var name) argsBodies)
  rec = traverse (test, ds)

dsPrefixLine = (test, ds) where
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
  rec = traverse (test, ds)

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

unusedVar :: Desugar Name
unusedVar = loop Nothing where
  toName Nothing = "_arg"
  toName (Just num) = "_arg" <> render num
  loop :: Maybe Int -> Desugar Name
  loop n = let name = toName n in
    isNameInUse (toName n) >>= \case
      True -> loop . Just $ case n of
        Nothing -> 0
        Just n -> n + 1
      False -> addUsedName name

isNameInUse :: Name -> Desugar Bool
isNameInUse name = do
  names <- get <!> dsNamesInUse
  loop names
  where loop [] = return False
        loop (names:ns) = case S.member name names of
          True -> return True
          False -> loop ns

defaultEnv = DesugarerEnv {
    dsNameSpace = mempty
  , dsConstructors = mempty
  , dsNamesInUse = mempty
  }

desugarers = [dsAfter, dsLambdaDot, dsPrefixLine, dsLambdas, dsDot]

desugar expr = go desugarers expr where
  go [] e = return e
  go (d:ds) e = traverse d e >>= go ds

log = lift . putStrLn
log' = log . mconcat

runDesugarWith :: DesugarerEnv -> Expr -> (Either ErrorList Expr, DesugarerEnv)
runDesugarWith state e =
  unsafePerformIO $ runStateT (runErrorT (desugar e)) state

runDesugarWith' ds state e =
  unsafePerformIO $ runStateT (runErrorT (traverse ds e)) state

runDesugar :: Expr -> Either ErrorList Expr
runDesugar = fst . runDesugarWith defaultEnv

runDesugar' ds expr = fst $ runDesugarWith' ds defaultEnv expr

desugarIt :: String -> Either ErrorList Expr
desugarIt input = case grab input of
  Left err -> error $ show err
  Right exprs -> runDesugar (Block exprs)

desugarIt' ds input = case grab input of
  Left err -> error $ show err
  Right exprs -> runDesugar' ds (Block exprs)
