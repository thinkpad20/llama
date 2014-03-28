module Desugar where

import qualified Data.Map as M
import qualified Data.Set as S

import Common
import AST
import TypeLib

data DesugarerEnv = DesugarerEnv {
    dsNameSpace :: [Name]
  , dsConstructors :: M.Map Name Type
  }
type Desugarer = StateT [S.Set Name] Identity

class Recursable a where
  recurse :: (a -> Bool) -> (a -> Desugarer a) -> a -> Desugarer a

instance Recursable Literal where
  recurse = undefined

instance Recursable Expr where
  recurse test f e | test e = f e
                   | otherwise = case e of
    Var _ -> return e
    Number _ -> return e
    String _ -> return e
    Constructor _ -> return e
    Continue -> return e
    TypeDef _ _ -> return e
    Block blk -> Block <$> mapM rec blk
    Dot e1 e2 -> Dot <$> rec e1 <*> rec e2
    Apply e1 e2 -> Apply <$> rec e1 <*> rec e2
    Lambda arg body -> Lambda <$> rec arg <*> rec body
    Lambdas argsBodies -> Lambdas <$> mapM recTup argsBodies
    Case expr patsBodies -> Case <$> rec expr <*> mapM recTup patsBodies
    Tuple exprs kws -> Tuple <$> mapM rec exprs <*> mapM recKw kws
    Literal (ArrayLiteral exprs) -> Literal . ArrayLiteral <$> mapM rec exprs
    Literal (ListLiteral exprs) -> Literal . ListLiteral <$> mapM rec exprs
    Literal (SetLiteral exprs) -> Literal . SetLiteral <$> mapM rec exprs
    DeRef e1 e2 -> DeRef <$> rec e1 <*> rec e2
    Typed expr t -> rec expr >>= \e' -> return $ Typed e' t
    If c t f -> If <$> rec c <*> rec t <*> rec f
    If' c t -> If' <$> rec c <*> rec t
    While c b -> While <$> rec c <*> rec b
    For e1 e2 e3 -> For <$> rec e1 <*> rec e2 <*> rec e3
    Define name expr -> Define name <$> rec expr
    Extend name expr -> Extend name <$> rec expr
    Assign e1 e2 -> Assign <$> rec e1 <*> rec e2
    Return expr -> Return <$> rec expr
    Throw expr -> Throw <$> rec expr
    Break expr -> Break <$> rec expr
    After e1 e2 -> After <$> rec e1 <*> rec e2
    Before e1 e2 -> Before <$> rec e1 <*> rec e2
    --ObjDec ObjectDec
    Modified m expr -> Modified m <$> rec expr
    where rec = recurse test f
          recTup (a,b) = (,) <$> rec a <*> rec b
          recKw = undefined

--desugarObject :: Expr -> Desugarer Expr
--desugarObject e = case e of

desugarBeforeAfter :: Expr -> Desugarer Expr
desugarBeforeAfter e = recurse test desugar e where
  test (After _ _) = True
  test (Before _ _) = True
  test _ = False
  desugar expr = case expr of
    e `After` (Block es) -> do
      es' <- mapM rec es
      e' <- rec e
      return $ Block $ es' ++ [e']
    e1 `After` e2 -> do
      e1' <- rec e1
      e2' <- rec e2
      return $ Block [e2', e1']
    _ `Before` _ -> error $ "Haven't implemented `Before` yet"
    where rec = desugarBeforeAfter

