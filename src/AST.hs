{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AST where

import Common

type Name = String
data Expr = Var Name
          | Number Double
          | String String
          | Dot Expr Expr
          | Apply Expr Expr
          | Unary String Expr
          | Lambda Expr Block
          | Binary String (Expr, Expr)
          | Tuple [Expr]
          | Array ArrayLiteral
          | Ref Expr Expr
          | Typed Expr Type
          deriving (Eq)

data Type = TVar Name
          | TConst Name
          | TTuple [Type]
          | TApply Type Type
          deriving (Eq)

data ArrayLiteral = ArrayLiteral [Expr] | ArrayRange Expr Expr deriving (Eq)

type Block = [Statement]
data Statement = Expr Expr
               | If Expr Block Block
               | If' Expr Block
               | While Expr Block
               | For Expr Expr Block
               | Define Expr Block
               | Assign Expr Block
               | Return Expr
               | Throw Expr
               | Break
               deriving (Eq)

instance Show Expr where
  show expr = case expr of
    Var name -> name
    Number n -> show n
    String s -> show s
    Dot e1 e2 -> show' e1 ++ "." ++ show' e2
    Apply e1 e2 -> show' e1 ++ " " ++ show' e2
    Tuple es -> "(" ++ (intercalate "," . map show) es ++ ")"
    Unary "~" e -> show $ Unary "-" e
    Unary op e -> op ++ show' e
    Binary op (e1, e2) -> show' e1 ++ " " ++ op ++ " " ++ show' e2
    Array (ArrayLiteral exprs) -> '[' : (intercalate ", " $ map show exprs) ++ "]"
    Array (ArrayRange start stop) -> '[' : show start ++ ".." ++ show stop ++ "]"
    Ref object index -> show' object ++ "{" ++ show index ++ "}"
    Lambda arg body -> show' arg ++ " => " ++ show'' body
    Typed expr typ -> show' expr ++ ": " ++ show typ
    where
      show'' [stmt] = show stmt
      show'' stmts = "{" ++ show stmts ++ "}"
      show' expr = case expr of
        Apply _ _ -> parens
        Dot _ _ -> parens
        Binary _ _ -> parens
        Unary _ _ ->  parens
        Lambda _ _ -> parens
        _ -> show expr
        where parens = "(" ++ show expr ++ ")"


instance Show Statement where
  show stmt = fst $ runState (render stmt) 0 where
    render :: Statement -> State Int String
    render stmt = case stmt of
      Expr expr -> line $ show expr
      If c t f -> do
        if' <- line $ "if " ++ show c
        else' <- line "else"
        t' <- block t
        f' <- block f
        join $ [if'] ++  t' ++ [else'] ++ f'
      If' c t -> do
        if' <- line $ "if " ++ show c
        t' <- block t
        join $ [if'] ++ t'
      While e blk -> do
        while <- line $ "while " ++ show e
        blk' <- block blk
        join $ [while] ++ blk'
      For pat expr blk -> do
        for <- line $ "for " ++ show pat ++ " in " ++ show expr
        blk' <- block blk
        join $ [for] ++ blk'
      Define e1 [Expr e2] -> line $ show e1 ++ " = " ++ show e2
      Define e blk -> do
        expr <- line $ show e ++ " ="
        body <- block blk
        join $ [expr] ++ body
      Assign e1 block -> line $ show e1 ++ " := " ++ show block
      Break -> line "break"
      Throw e -> line $ "throw " ++ show e
      Return e -> line $ "return " ++ show e
    line str = get >>= \i -> return $ replicate i ' ' ++ str
    join = return . intercalate "\n"
    block blk = up *> mapM render blk <* down
    (up, down) = (modify (+2), modify (\n -> n - 2))

instance Show Block where
  show = unlines . map show

instance Show Type where
  show t = case t of
    TVar name -> name
    TConst name -> name
    TTuple ts -> "(" ++ (intercalate ", " $ map show ts) ++ ")"
    TApply t1 t2 -> show' t1 ++ " " ++ show' t2
    where show' typ = case typ of
            TApply _ _ -> "(" ++ show typ ++ ")"
            _ -> show typ























