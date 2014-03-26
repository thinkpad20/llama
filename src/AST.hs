{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module AST where

import qualified Data.Text as T

import Common
import TypeLib

data Expr = Var         !Name
          | Number      !Double
          | String      !T.Text
          | Constructor !Name
          | Block       !Block
          | Dot         !Expr !Expr
          | Apply       !Expr !Expr
          | Unary       !String !Expr
          | Lambda      !Expr !Expr
          | Lambdas     ![(Expr, Expr)]
          | Case        !Expr ![(Expr, Expr)]
          | Tuple       ![Expr]
          | Literal     !Literal
          | DeRef       !Expr !Expr
          | Typed       !Expr !Type
          | If          !Expr !Expr !Expr
          | If'         !Expr !Expr
          | While       !Expr !Expr
          | For         !Expr !Expr !Expr
          | Define      !Name !Expr
          | Extend      !Name !Expr
          | Assign      !Expr !Expr
          | Return      !Expr
          | Throw       !Expr
          | Break       !Expr
          | After       !Expr !Expr
          | Before      !Expr !Expr
          | Continue
          | Modified    !Mod !Expr
          | TypeDef !Name !Type
          deriving (Show, Eq)

data Literal = ArrayLiteral ![Expr]
             | ArrayRange   !Expr !Expr
             | DictLiteral  ![(Expr, Expr)]
             | SetLiteral   ![Expr]
             | ListLiteral  ![Expr]
             deriving (Show, Eq)

type Block = [Expr]

instance Render Expr where
  render e = evalState (render' e) 0 where
    render' :: Expr -> State Int T.Text
    render' stmt = case stmt of
      Var name -> return name
      Constructor name -> return name
      Number n | isInt n -> return $ T.pack $ show $ floor n
      Number n -> return $ T.pack $ show n
      Block blk -> block blk
      String s -> return $ T.pack $ show s
      Dot e1 e2 -> return $ render'' e1 <> "." <> render'' e2
      Apply (Var op) (Tuple [e1, e2])
        | isSymbol op -> return $ render'' e1 <> " " <> op <> " " <> render'' e2
      Apply (Var op) e | isSymbol op -> return $ op <> " " <> render'' e
      Apply e1 e2 -> return $ render'' e1 <> " " <> render'' e2
      Tuple es -> return $ "(" <> (T.intercalate "," . map render) es <> ")"
      Literal (ArrayLiteral exprs) -> return $ "[" <> T.intercalate ", " (map render exprs) <> "]"
      Literal (ArrayRange start stop) -> return $ "[" <> render start <> ".." <> render stop <> "]"
      DeRef object index -> return $ render'' object <> "[:" <> render index <> "]"
      Lambda arg expr -> return $ render'' arg <> " => " <> render expr
      Case expr alts -> return $ "case " <> render expr <> " of " <> T.intercalate " | " rPairs
        where rPairs = map (\(p, r) -> render p <> " => " <> render r) alts
      Typed expr typ -> return $ render'' expr <> ": " <> render typ
      If c t f -> do
        if' <- line $ "if " <> render c
        else' <- line "else"
        t' <- render' t
        f' <- render' f
        join [if', t', else', f']
      If' c t -> do
        if' <- line $ "if " <> render c
        t' <- render' t
        join [if', t']
      While ex blk -> do
        while <- line $ "while " <> render ex
        blk' <- render' blk
        join [while, blk']
      For pat expr blk -> do
        for <- line $ "for " <> render pat <> " in " <> render expr
        blk' <- render' blk
        join [for, blk']
      Define name expr -> line $ name <> " = " <> render expr
      Extend name expr -> line $ name <> " &= " <> render expr
      Assign e1 block -> line $ render e1 <> " := " <> render block
      Break e -> line $ "break " <> render e
      Throw e -> line $ "throw " <> render e
      Return e -> line $ "return " <> render e
      TypeDef name typ -> line $ "typedef " <> name <> " = " <> render typ
      _ -> return $ T.pack $ show stmt
    line str = get >>= \i -> return $ T.replicate i " " <> str
    join = return . T.intercalate "\n"
    block blk = up *> fmap (T.intercalate "; ") (mapM render' blk) <* down
    (up, down) = (modify (+2), modify (\n -> n - 2))
    render'' expr = case expr of
      Apply _ _ -> parens
      Dot _ _ -> parens
      Lambda _ _ -> parens
      _ -> render expr
      where parens = "(" <> render expr <> ")"

instance Render Block where
  render b = "{" <> (line . trim . T.intercalate "; " . map render) b <> "}"

symChars :: String
symChars = "><=+-*/^~!%@&$:.#|?"

isSymbol :: T.Text -> Bool
isSymbol = T.all (`elem` symChars)

binary :: Name -> Expr -> Expr -> Expr
binary name e1 e2 = Apply (Var name) $ Tuple [e1, e2]
