module Desugar where

import qualified Data.Map as M
import qualified Data.Set as S

import Common
import AST
import Parser

type Desugarer = StateT [S.Set Name] Identity

desugarBeforeAfter :: Expr -> Desugarer Expr
desugarBeforeAfter expr = case expr of
  e `After` (Block es) -> do
    es' <- mapM rec es
    e' <- rec e
    return $ Block $ es' ++ [e']
  e1 `After` e2 -> do
    e1' <- rec e1
    e2' <- rec e2
    return $ Block [e2', e1']
  where rec = desugarBeforeAfter
