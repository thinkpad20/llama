{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module ParserLib (indent, dedent, same, parse, Parser, ParserState(..),
               ignoreOn, ignoreOff, toIgnore, ignoring) where

import Prelude (IO, Eq(..), Ord(..), Bool(..),
                Double, String, Maybe(..), Int, Monad(..),
                ($), (.), floor, map, Functor(..), mapM,
                (+), (-), elem, Either(..), length, unwords)

import qualified Prelude as P
import Common
import Text.Parsec hiding (parse)
import "mtl" Control.Monad.Identity

-- These are for testing only and not exported
data Expression = Id String deriving (P.Show)
type Block = [Statement]
data Statement = Expr Expression
               | While Expression Block
               deriving (P.Show)

data ParserState = ParserState { currentLevel :: Int
                               , stepAmount :: Maybe Int
                               , raNames :: [Name]
                               , ignoreLambda :: Bool }
type Parser = ParsecT String ParserState Identity

getLevel :: Parser Int
getLevel = currentLevel <$> getState
setLevel :: Int -> Parser ()
setLevel level = modifyState $ \s -> s { currentLevel = level }
getStepAmount :: Parser (Maybe Int)
getStepAmount = stepAmount <$> getState
setStepAmount :: Int -> Parser ()
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
                          "Wrong indentation step: was recorded as", P.show step'
                        , "but found an indentation of", P.show step]
      False -> unexpected $ unwords ["Not an indent: looked for more than"
                                    , P.show level, "spaces, but found"
                                    , P.show nspaces]
dedent = eof <|> (newline >> go) where
  go = lookAhead $ do
    nspaces <- length <$> (many $ char ' ')
    level <- getLevel
    case nspaces < level of
      True -> setLevel nspaces
      False -> unexpected $ unwords ["Not a dedent: looked for less than"
                                    , P.show level, "spaces, but found"
                                    , P.show nspaces]

same = (many emptyLine >> eof) <|> (newline >> go) where
  go = try $ do
    nspaces <- length <$> (many $ char ' ')
    level <- getLevel
    case nspaces == level of
      True -> return ()
      False -> unexpected $ unwords ["Not the same indentation:"
                                    , "looked for", P.show level
                                    , "spaces, but found", P.show nspaces]

defaultState :: ParserState
defaultState = ParserState { currentLevel = 0
                           , stepAmount = Nothing
                           , raNames = ["print", "println", "assert"]
                           , ignoreLambda = False }

parse :: Parser a -> String -> Either ParseError a
parse parser input = runIdentity $ runParserT parser defaultState "" input

ignoreOn, ignoreOff :: Parser ()
ignoreOn = modifyState $ \s -> s {ignoreLambda = True}
ignoreOff = modifyState $ \s -> s {ignoreLambda = False}

toIgnore :: Parser Bool
toIgnore = getState <!> ignoreLambda

ignoring :: Parser a -> Parser a
ignoring p = ignoreOn *> p <* ignoreOff
