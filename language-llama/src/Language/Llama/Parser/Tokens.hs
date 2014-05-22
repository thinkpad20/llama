{-# LANGUAGE OverloadedStrings #-}
module Language.Llama.Parser.Tokens (
    Token(..), IStr(..), Token'(..), keywords
  ) where

import qualified Prelude as P
import Data.Char (toUpper)
import qualified Data.Text as T

import Language.Llama.Common.Common

data Token t = TId Name
             | TInt Integer
             | TFloat Double
             | TStr Text
             | TIStr (IStr t)
             | TSymbol Name
             | TPunc Char
             | Indent
             | Outdent
             | Nodent
             | TLineComment Text
             | TBlockComment Text
             | TKeyword Text
             | TRaw Text
             | TRegex Text
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
    TId name -> "IDENT: " <> name
    TInt int -> "INT: " <> render int
    TFloat dub -> "FLOAT: " <> render dub
    TStr txt -> "STR: " <> render txt
    TIStr istr -> "ISTR: " <> render istr
    TSymbol nm -> "SYM: " <> render nm
    TPunc chr -> "PUNC: " <> render chr
    Indent -> "INDENT"
    Outdent -> "OUTDENT"
    Nodent -> "NODENT"
    TLineComment txt -> "LCOMMENT: " <> txt
    TBlockComment txt -> "BCOMMENT: " <> txt
    TKeyword txt -> "KEYWORD: " <> txt
    TRaw txt -> "RAW: " <> txt
    TRegex txt -> "REGEX: " <> txt

addChar :: IStr a -> Char -> IStr a
addChar (Plain t) c = Plain $ t `snoc` c
addChar (IStr is t is') c = IStr is t (is' `addChar` c)

keywords :: [Name]
keywords = ["case","catch","class","else","finally","for","if","in","return",
            "of","then","try","unless","with","while", "object", "forever",
            "do"]
