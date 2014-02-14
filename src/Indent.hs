{-# LANGUAGE LambdaCase #-}
module Indent (indent, dedent, same, parse, Parser(..)) where

import Text.Parsec hiding (parse)
import Control.Applicative hiding (many, (<|>))
import Control.Monad.Identity

-- These are for testing only and not exported
data Expression = Id String deriving (Show)
type Block = [Statement]
data Statement = Expr Expression
               | While Expression Block
               deriving (Show)

type Parser = ParsecT String Int Identity

lexeme :: Parser a -> Parser a
lexeme p = p <* many (oneOf " \t")

sstring :: String -> Parser String
sstring = lexeme . string

schar :: Char -> Parser Char
schar = lexeme . char

getLevel = getState
setLevel = setState

indent, dedent, same, emptyLine :: Parser ()
emptyLine = try $ many (oneOf " \t") >> char '\n' >> return ()
indent = try $ do
  many emptyLine
  -- now we're at the beginning of a non-empty line, see if # spaces > level
  nspaces <- length <$> (many $ char ' ')
  level <- getLevel
  case nspaces > level of
    True -> setLevel nspaces
    False -> unexpected $ "Not an indent: was " ++ show level ++ ", now " ++ show nspaces

dedent = try $ do
  many emptyLine
  nspaces <- length <$> (many $ char ' ')
  level <- getLevel
  case nspaces < level of
    True -> setLevel nspaces
    False -> unexpected "Not a dedent"

same = try $ do
  many emptyLine
  nspaces <- length <$> (many $ char ' ')
  level <- getLevel
  case nspaces == level of
    True -> return ()
    False -> unexpected$ "Not the same indent: was " ++ show level ++ ", now " ++ show nspaces

block :: Parser Block
block = indent *> statements <* dedent

while :: Parser Statement
while = While <$ sstring "while" <*> expression <*> block

expression :: Parser Expression
expression = Id <$> many1 letter

statement :: Parser Statement
statement = choice [while, Expr <$> expression]

statements :: Parser [Statement]
statements = statement `sepBy1` same

parse parser input = runIdentity $ runParserT parser 0 "" input
