{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AST where

import Common

data Expr = Var Name
          | Number Double
          | String String
          | Constructor Name
          | Block Block
          | Dot Expr Expr
          | Apply Expr Expr
          | Unary String Expr
          | Lambda Expr Expr
          | Case Expr [(Expr, Expr)]
          | Tuple [Expr]
          | Array ArrayLiteral
          | Ref Expr Expr
          | Typed Expr Type
          deriving (Show, Eq)

-- | A variable can be rigid (fixed in scope), or polymorphic (free to take on
-- multiple forms in the same scope).
data TVarType = Rigid | Polymorphic deriving (Show, Eq, Ord)

data Type = TVar TVarType Name
          | TConst Name [Type]
          | TFunction Type Type
          deriving (Show, Eq, Ord)

tTuple = TConst ""
tConst name = TConst name []

(==>) = TFunction
infixr 4 ==>

data ArrayLiteral = ArrayLiteral [Expr]
                  | ArrayRange Expr Expr deriving (Show, Eq)

type Block = [Statement]
data Statement = Expr Expr
               | If Expr Block Block
               | If' Expr Block
               | While Expr Block
               | For Expr Expr Block
               | Define Name Expr
               | Extend Name Expr
               | Assign Expr Expr
               | Return Expr
               | Throw Expr
               | Break
               | Continue
               deriving (Show, Eq)

instance Render Expr where
  render expr = case expr of
    Var name -> name
    Constructor name -> name
    Number n | isInt n -> show $ floor n
    Number n -> show n
    Block blk -> render'' blk
    String s -> show s
    Dot e1 e2 -> render' e1 ++ "." ++ render' e2
    Apply (Var op) (Tuple [e1, e2])
      | isSymbol op -> render' e1 ++ " " ++ op ++ " " ++ render' e2
    Apply (Var "~") e -> "-" ++ render' e
    Apply (Var op) e | isSymbol op ->  op ++ " " ++ render' e
    Apply e1 e2 -> render' e1 ++ " " ++ render' e2
    Tuple es -> "(" ++ (intercalate "," . map render) es ++ ")"
    Array (ArrayLiteral exprs) -> '[' : (intercalate ", " $ map render exprs) ++ "]"
    Array (ArrayRange start stop) -> '[' : render start ++ ".." ++ render stop ++ "]"
    Ref object index -> render' object ++ "[:" ++ render index ++ "]"
    Lambda arg expr -> render' arg ++ " => " ++ render expr
    Case expr alts -> "case " ++ render expr ++ " of " ++ intercalate " | " rPairs
      where rPairs = map (\(p, r) -> render p ++ " => " ++ render r) alts
    Typed expr typ -> render' expr ++ ": " ++ render typ
    where
      render'' [stmt] = render stmt
      render'' stmts = "{" ++ render stmts ++ "}"
      render' expr = case expr of
        Apply _ _ -> parens
        Dot _ _ -> parens
        Lambda _ _ -> parens
        _ -> render expr
        where parens = "(" ++ render expr ++ ")"


instance Render Statement where
  render stmt = fst $ runState (render' stmt) 0 where
    render' :: Statement -> State Int String
    render' stmt = case stmt of
      Expr expr -> line $ render expr
      If c t f -> do
        if' <- line $ "if " ++ render c
        else' <- line "else"
        t' <- block t
        f' <- block f
        join $ [if'] ++  t' ++ [else'] ++ f'
      If' c t -> do
        if' <- line $ "if " ++ render c
        t' <- block t
        join $ [if'] ++ t'
      While e blk -> do
        while <- line $ "while " ++ render e
        blk' <- block blk
        join $ [while] ++ blk'
      For pat expr blk -> do
        for <- line $ "for " ++ render pat ++ " in " ++ render expr
        blk' <- block blk
        join $ [for] ++ blk'
      Define name expr -> line $ name ++ " = " ++ render expr
      Extend name expr -> line $ name ++ " &= " ++ render expr
      Assign e1 block -> line $ render e1 ++ " := " ++ render block
      Break -> line "break"
      Throw e -> line $ "throw " ++ render e
      Return e -> line $ "return " ++ render e
    line str = get >>= \i -> return $ replicate i ' ' ++ str
    join = return . intercalate "\n"
    block blk = up *> mapM render' blk <* down
    (up, down) = (modify (+2), modify (\n -> n - 2))

instance Render Block where
  render b = "{" ++ (line . trim . (intercalate "; ") . map render) b ++ "}"

instance Render Type where
  render t = case t of
    TVar Rigid name -> name
    TVar Polymorphic name -> "(some " ++ name ++ ")"
    TConst "" ts -> "(" ++ (intercalate ", " $ map render ts) ++ ")"
    TConst name [] -> name
    TConst "[]" [t] -> "[" ++ render t ++ "]"
    TConst "[!]" [t] -> "[!" ++ render t ++ "]"
    TConst name [t] -> name ++ " " ++ render' t
    TConst name ts -> name ++ " " ++ "(" ++ (intercalate ", " $ map render ts) ++ ")"
    TFunction t1 t2 -> render'' t1 ++ " -> " ++ render t2
    where render' typ = case typ of
            TConst _ _ -> "(" ++ render typ ++ ")"
            _ -> render typ
          render'' typ = case typ of
            TFunction _ _ -> "(" ++ render typ ++ ")"
            _ -> render typ

symChars = "><=+-*/^~!%@&$:.#|?"
isSymbol = all (`elem` symChars)
binary name e1 e2 = Apply (Var name) $ Tuple [e1, e2]

boolT = tConst "Bool"
numT = tConst "Num"
strT = tConst "Str"
arrayOf a = TConst "[]" [a]
listOf a = TConst "[!]" [a]
unitT = tTuple []
