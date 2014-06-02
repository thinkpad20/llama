{-# LANGUAGE OverloadedStrings #-}
module Language.Llama.JavaScript.AST (Statement(..),
                       Expr(..),
                       Block(..),
                       throwNewError,
                       emptyObj) where

import qualified Prelude as P
import Data.HashMap.Strict hiding (map)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T

import Control.Applicative
import Language.Llama.Common.Common

data Block = Block [Statement] deriving (P.Show, Eq)

data Statement
  = Expr Expr
  | If Expr Block (Maybe Block)
  | While Expr Block
  | For Statement Statement Statement Block
  | Declare [Name]
  | DeclareAssign Name Expr
  | Return Expr
  | Return'
  | Throw Expr
  | Break
  deriving (P.Show, Eq)

data Expr
  = Number Double
  | Var Name
  | String Text
  | Array [Expr]
  | Assign Expr Expr
  | Keyword Name
  | Function [Name] Block
  | Parens Expr
  | Dot Expr Name
  | Call Expr [Expr]
  | ArrayReference Expr Expr
  | Object (HashMap Name Expr)
  | Binary Text Expr Expr
  | Unary Text Expr
  | Ternary Expr Expr Expr
  | New Expr
  deriving (P.Show, Eq)

emptyObj :: Expr
emptyObj = Object mempty

instance Render Expr where
  render t = case t of
    Number n | isInt n -> render (floor n :: Int)
             | True -> render n
    Var n -> n
    String s -> render s
    Keyword n -> n
    Function ns blk -> c ["function(", sep ns, "){", render blk, "}"]
    Array exprs -> c ["[", sep $ map render exprs, "]"]
    Dot e name -> render e <> "." <> name
    Call f@(Function _ _) es ->
      c ["(", render f, ")(", sep (map render es), ")"]
    Call e es -> c [render e, "(", sep (map render es), ")"]
    Binary op e1 e2 -> c [render e1, op, render e2]
    Unary op e -> op <> render e
    Ternary e1 e2 e3 -> c ["(", render e1, "?", render e2,
                                    ":", render e3, ")"]
    ArrayReference e1 e2 -> render e1 <> "[" <> render e2 <> "]"
    New e -> "new " <> render e
    Assign (Var n) (Function ns blk) ->
      c ["function ", n, "(", sep ns, "){", render blk, "}"]
    Assign e e' -> render e <> "=" <> render e'
    Object o -> toJsObject o
    where c   = mconcat
          sep = T.intercalate ","
  renderI n e = mconcat $ case e of
    Number n -> [if isInt n then render (floor n :: Integer) else render n]
    Var n -> [n]
    Keyword n -> [n]
    String s -> [render s]
    Assign (Var v) e@(Function _ _) -> [v, " = ", renderI n e]
    Assign (Var v) e -> [v, " = ", renderI n e]
    Assign e e' -> [renderI n e, " = ", renderI n e']
    Function ns blk -> ["function(", sep ns, ") {\n",
                         renderI (n+1) blk, sp n "}"]
    Array exprs -> ["[", sep $ map (renderI n) exprs, "]"]
    Dot e name -> [renderI n e, ".", name]
    Call e es -> [renderI n e, "(", sep (map (renderI n) es), ")"]
    ArrayReference e e' -> [renderI n e, "[", renderI n e', "]"]
    Binary op e1 e2 -> [renderI n e1, " ", op, " ", renderI n e2]
    Unary op e -> [op, renderI n e]
    Ternary e1 e2 e3 -> ["(", renderI n e1, " ? ", renderI n e2, " : ",
                           renderI n e3, ")"]
    New e -> ["new ", renderI n e]
    where sep = T.intercalate ", "
          sp n s = "\n" <> T.replicate (n * indentation) " " <> s

toJsObject :: HashMap Name Expr -> Text
toJsObject mp = do
  let doit = each (keys mp) $ \k -> k <> ":" <> render (mp H.! k)
  "{" <> T.intercalate "," doit <> "}"

instance Render Statement where
  render s = case s of
    Expr e -> render e
    If e b1 b2 -> do
      let _if = "if(" <> render e <> "){" <> render b1
          _then = "{" <> render b1 <> "}"
          _else = renderElse b2
          renderElse Nothing = ""
          renderElse (Just (Block [i@(If _ _ _)])) = " " <> render i
          renderElse (Just b2) = "{" <> render b2 <> "}"
      _if <> _then <> _else
    DeclareAssign name e -> "var " <> name <> " = " <> render e
    While e b -> "while(" <> render e <> "){" <> render b <> "}"
    For e1 e2 e3 b -> c ["for(", render e1,";", render e2, ";", render e3,
                             "){", render b, "}"]
    Return' -> "return"
    Return e -> "return " <> render e
    Break -> "break"
    Throw e -> "throw " <> render e
    Declare ns -> "var " <> T.intercalate "," ns
    where c = mconcat
  renderI n stmt = mconcat $ case stmt of
    If e b1 b2 -> ["if (", renderI n e, ") {", rec b1, sp n "}",
      case b2 of
        Nothing -> ""
        Just (Block [i@(If e block1 block2)]) -> " else " <> renderI n i
        Just b2 -> " else {" <> rec b2 <> sp n "}"]
    DeclareAssign name e -> ["var ", name, " = ", renderI n e]
    While e b -> ["while (", renderI n e, ") {", rec b, sp n "}"]
    For e1 e2 e3 b -> ["for (", renderI n e1, ";", renderI n e2,
                         ";", renderI n e3, ") {", rec b, sp n "}"]
    Declare names -> ["var ", T.intercalate ", " names, ";\n"]
    Return' -> ["return;"]
    Return e -> ["return ", renderI n e, ";"]
    Break -> ["break;"]
    Expr e -> [renderI n e, ";\n"]
    Throw e -> ["throw ", renderI n e, ";"]
    where sp n s = "\n" <> T.replicate (n * indentation) " " <> s
          rec block = "\n" <> renderI (n+1) block

instance Render Block where
  render (Block stmts) = T.intercalate ";" (map render stmts) <> ";"
  renderI n b = r n b where
    r n (Block stmts) = mconcat $ map (sp n . renderI n) stmts
    sp n s = T.replicate (n * indentation) " " <> s

instance Monoid Block where
  mempty = Block []
  mappend (Block a) (Block b) = Block (a <> b)

indentation :: Int
indentation = 2

throwNewError msg = Throw $ New $ Call (Var "Error") [String msg]

