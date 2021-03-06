{-# LANGUAGE OverloadedStrings #-}
module Language.Llama.Parser.Tokens (
    Token(..), IStr(..), Token'(..), keywords
  ) where

import qualified Prelude as P
import Prelude (Show)
import Data.Char (toUpper)
import qualified Data.Text as T

import Language.Llama.Common.Common

data Token t = TId Name
             | TInt Integer
             | TFloat Double
             | TStr Text
             | TKeyword Text
             | TIStr (IStr t)
             | TSymbol Name
             | TPunc Char
             | Indent
             | Outdent
             | Nodent
             | TLineComment Text
             | TBlockComment Text
             deriving (Show, Eq)

data IStr t = Plain Text
            | IStr (IStr t) [t] (IStr t)
            deriving (Show, Eq)

instance Monoid (IStr t) where
  mempty = Plain mempty
  is1 `mappend` is2 = case (is1, is2) of
    (Plain s, Plain s') -> Plain (s <> s')
    (s, IStr is t is') -> IStr (s <> is) t is'
    (IStr is t is', s) -> IStr is t (is' <> s)

newtype Token' = Token' (Token Token') deriving (Show, Eq)

instance Render t => Render (IStr t) where
  render (Plain txt) = render txt
  render (IStr is ts is') = do
    let ts' = T.intercalate ", " $ map render ts
    "Interpolated(" <> render is <> ", " <> ts' <> ", " <> render is' <> ")"

instance Render t => Render (Token t) where
  render tkn = case tkn of
    TId name          -> "IDENTIFIER: " <> name
    TKeyword txt      -> "KEYWORD: " <> txt
    TInt int          -> "INT: " <> render int
    TFloat dub        -> "FLOAT: " <> render dub
    TStr txt          -> "STR: " <> render txt
    TIStr istr        -> "ISTR: " <> render istr
    TSymbol nm        -> "SYMBOL: " <> render nm
    TPunc chr         -> "CHAR: " <> render chr
    Indent            -> "INDENT"
    Outdent           -> "OUTDENT"
    Nodent            -> "NODENT"
    TLineComment txt  -> "LINE COMMENT: " <> txt
    TBlockComment txt -> "BLOCK COMMENT: " <> txt

keywords :: [Name]
keywords = ["case","catch","class","else","finally","for","if","in","return",
            "of","then","try","unless","with","while","type","forever","do",
            "trait","implement","before","after","ref","array","hash","json"]
