{-# LANGUAGE OverloadedStrings #-}
module JavaScript.AST (Statement(..),
                       Expr(..),
                       Block(..),
                       throwNewError) where

import qualified Prelude as P
import Prelude (IO, Eq(..), Ord(..), Bool(..), Show(..),
                Double, String, Maybe(..), Int, Monad(..),
                ($), (.), floor, map, Functor(..), mapM,
                (+), (-), (*), elem, Either(..), Integer)
import Data.Monoid
import Data.Text hiding (map)
import qualified Data.List as L

import Control.Applicative
import Common hiding (intercalate, show)

data Block = Block [Name] [Statement]

data Statement =
  Expr Expr
  | If Expr Block Block
  | If' Expr Block
  | While Expr Block
  | For Expr Expr Expr Block
  | Declare Name
  | Return Expr
  | Return'
  | Throw Expr
  | Break

data Expr =
  Bool Bool
  | Number Double
  | Var Name
  | String Text
  | Array [Expr]
  | Assign Expr Expr
  | This
  | Function [Name] Block
  | Parens Expr
  | Dot Expr Expr
  | Call Expr [Expr]
  | ArrayReference Expr Expr
  | Binary Text Expr Expr
  | Unary Text Expr
  | Ternary Expr Expr Expr
  | New Expr

instance Show Expr where
  show t = case t of
    Number n | isInt n -> show $ floor n | True -> show n
    Var n -> unpack n
    String s -> show s
    This -> "this"
    Function ns blk -> c ["function(", sep $ map unpack ns, "){", show blk, "}"]
    Bool True -> "true"
    Bool False -> "false"
    Array exprs -> c ["[", sep $ map show exprs, "]"]
    Dot e1 e2 -> show e1 <> "." <> show e2
    Call f@(Function _ _) es ->
      c ["(", show f, ")(", sep (map show es), ")"]
    Call e es -> c [show e, "(", sep (map show es), ")"]
    Binary op e1 e2 -> c [show e1, unpack op, show e2]
    Unary op e -> unpack op <> show e
    Ternary e1 e2 e3 -> c ["(", show e1, "?", show e2,
                                    ":", show e3, ")"]
    ArrayReference e1 e2 -> show e1 <> "[" <> show e2 <> "]"
    New e -> "new " <> show e
    Assign e e' -> show e <> " = " <> show e'
    where c   = mconcat
          sep = L.intercalate ","

instance Show Statement where
  show s = case s of
    Expr e -> show e <> ";"
    If' e b -> c ["if(", show e, "){", show b, "}"]
    If e b1 b2 -> c [show (If' e b1), "else", showElse b2] where
      showElse (Block _ [_if@(If _ _ _)]) = " " <> show _if
      showElse _ = "{" <> show b2 <> "}"
    While e b -> "while(" <> show e <> "){" <> show b <> "}"
    For e1 e2 e3 b -> c ["for(", show e1,";", show e2, ";", show e3,
                             "){", show b, "}"]
    Return' -> "return;"
    Return e -> "return " <> show e <> ";"
    Break -> "break;"
    Throw e -> "throw " <> show e <> ";"
    Declare v -> "var " <> unpack v <> ";"
    where c = mconcat

instance Show Block where
  show = blockToBlock ~> s where
    s (Block _ stmts) = mconcat $ map show stmts

instance Monoid Block where
  mempty = Block [] []
  mappend (Block ns a) (Block ns' b) = Block (ns <> ns') (a <> b)

blockToBlock :: Block -> Block
blockToBlock (Block names stmts) = do
  let decs = map Declare names
  Block [] $ decs <> stmts

indentation :: Int
indentation = 2

instance Render Statement where
  renderI n stmt = mconcat $ case stmt of
    If' e b -> ["if (", renderI n e, ") {", rec b, sp n "}"]
    If e b1 b2 -> ["if (", renderI n e, ") {", rec b1, sp n "}",
      case b2 of
        Block _ [i@(If e block1 block2)] -> " else " <> renderI n i
        _ -> " else {" <> rec b2 <> sp n "}"]
    While e b -> ["while (", renderI n e, ") {", rec b, sp n "}"]
    For e1 e2 e3 b -> ["for (", renderI n e1, ";", renderI n e2,
                         ";", renderI n e3, ") {", rec b, sp n "}"]
    Declare name -> ["var ", name, ";\n"]
    Return' -> ["return;"]
    Return e -> ["return ", renderI n e, ";"]
    Break -> ["break;"]
    Expr e -> [renderI n e, ";\n"]
    Throw e -> ["throw ", renderI n e, ";"]
    where sp n s = "\n" <> replicate (n * indentation) " " <> s
          rec block = "\n" <> renderI (n+1) block

instance Render Block where
  renderI n b = r n (blockToBlock b) where
    r n (Block _ stmts) = mconcat $ map (sp n . renderI n) stmts
    sp n s = replicate (n * indentation) " " <> s

instance Render Expr where
  renderI n e = mconcat $ case e of
    Number n -> [if isInt n then render (floor n :: Integer) else render n]
    Var n -> [n]
    String s -> [render s]
    This -> ["this"]
    Bool True -> ["true"]
    Bool False -> ["false"]
    Assign (Var v) e@(Function _ _) -> [v, " = ", renderI n e]
    Assign (Var v) e -> [v, " = ", renderI n e]
    Assign e e' -> [renderI n e, " = ", renderI n e']
    Function ns blk -> ["function (", sep ns, ") {\n",
                         renderI (n+1) blk, sp n "}"]
    Array exprs -> ["[", sep $ map (renderI n) exprs, "]"]
    Dot e1 e2 -> [renderI n e1, ".", renderI n e2]
    Call e es -> [renderI n e, "(", intercalate ", " (map (renderI n) es), ")"]
    ArrayReference e e' -> [renderI n e, "[", renderI n e', "]"]
    Binary op e1 e2 -> [renderI n e1, " ", op, " ", renderI n e2]
    Unary op e -> [op, renderI n e]
    Ternary e1 e2 e3 -> ["(", renderI n e1, " ? ", renderI n e2, " : ",
                           renderI n e3, ")"]
    New e -> ["new ", renderI n e]
    where sep = intercalate ", "
          sp n s = "\n" <> replicate (n * indentation) " " <> s

throwNewError msg = Throw $ New $ Call (Var "Error") [String msg]

