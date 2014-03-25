{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserLib (indent, dedent, same, parse, Parser(..), IndentState(..),
               ignoreOn, ignoreOff, toIgnore, ignoring) where

import Common
import Text.Parsec hiding (parse)
import Control.Monad.Identity

-- These are for testing only and not exported
data Expression = Id String deriving (Show)
type Block = [Statement]
data Statement = Expr Expression
               | While Expression Block
               deriving (Show)

data IndentState = IndentState { currentLevel :: Int
                               , stepAmount :: Maybe Int
                               , raNames :: [Name]
                               , ignoreLambda :: Bool }
type Parser = ParsecT String IndentState Identity

lexeme :: Parser a -> Parser a
lexeme p = p <* many (oneOf " \t")

sstring :: String -> Parser String
sstring = lexeme . string

schar :: Char -> Parser Char
schar = lexeme . char

getLevel = currentLevel <$> getState
setLevel level = modifyState $ \s -> s { currentLevel = level }
getStepAmount = stepAmount <$> getState
setStepAmount step = modifyState $ \s -> s { stepAmount = Just step }

indent, dedent, same, emptyLine :: Parser ()
emptyLine = try $ many (oneOf " \t") >> char '\n' >> return ()
indent = newline >> go where
  go = try $ do
    -- now we're at the beginning of a non-empty line, see if # spaces > level
    nspaces <- length <$> (many $ char ' ')
    level <- getLevel
    let step = nspaces - level
    case nspaces > level of
      True -> getStepAmount >>= \case
        Nothing -> setLevel nspaces >> setStepAmount step
        Just step' -> if step == step' then setLevel nspaces
                      else unexpected $ unwords [
                          "Wrong indentation step: was recorded as", show step'
                        , "but found an indentation of", show step]
      False -> unexpected $ unwords ["Not an indent: looked for more than"
                                    , show level, "spaces, but found"
                                    , show nspaces]
dedent = eof <|> (newline >> go) where
  go = lookAhead $ do
    nspaces <- length <$> (many $ char ' ')
    level <- getLevel
    case nspaces < level of
      True -> setLevel nspaces
      False -> unexpected $ unwords ["Not a dedent: looked for less than"
                                    , show level, "spaces, but found"
                                    , show nspaces]

same = (many emptyLine >> eof) <|> (newline >> go) where
  go = try $ do
    nspaces <- length <$> (many $ char ' ')
    level <- getLevel
    case nspaces == level of
      True -> return ()
      False -> unexpected $ unwords ["Not the same indentation:"
                                    , "looked for", show level
                                    , "spaces, but found", show nspaces]

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

defaultState = IndentState { currentLevel = 0
                           , stepAmount = Nothing
                           , raNames = ["print", "assert"]
                           , ignoreLambda = False }
parse parser input = runIdentity $ runParserT parser defaultState "" input

ignoreOn, ignoreOff :: Parser ()
ignoreOn = modifyState $ \s -> s {ignoreLambda = True}
ignoreOff = modifyState $ \s -> s {ignoreLambda = False}

toIgnore :: Parser Bool
toIgnore = getState <!> ignoreLambda

ignoring :: Parser a -> Parser a
ignoring p = ignoreOn *> p <* ignoreOff
