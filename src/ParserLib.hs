{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
module ParserLib (indent, dedent, same, parse, Parser, ParserState(..),
               ignoreOn, ignoreOff, toIgnore, ignoring) where

import qualified Prelude as P
import qualified Data.Text as T
import Data.Set hiding (map, toList)
import Text.Parsec hiding (parse, spaces)

import Common
import AST

data ParserState = ParserState { indents :: [Int]
                               , raNames :: [Name]
                               , debugIndent :: Int
                               , ignoreLambda :: Bool }
type Parser = ParsecT String ParserState Identity

---------------------------------------------------------
-----------------------  Indentation  -------------------
---------------------------------------------------------

-- | Succeeds if indentation has increased.
indent :: Parser ()
indent = logged "an indent" $ try $ do
  newline
  newIndent <- length <$> many (char ' ')
  debug $ "the new indent is " <> render newIndent
  oldIndent:_ <- indents <$> getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent
    False -> unexpected "Not an indent"
  where pushIndent i = modifyState $ \s -> s{indents = i: indents s}

-- | Succeeds if indentation has decreased. Doesn't consume input.
outdent :: Parser ()
outdent = logged "an outdent" $ try $ do
  isOutdent <- (eof >> return True) <|> lookAhead (
    do newline
       newIndent <- length <$> many (char ' ')
       oldIndent:_ <- indents <$> getState
       return (newIndent < oldIndent))
  case isOutdent of
    True -> popIndent
    False -> unexpected "Not an outdent"
  where popIndent = modifyState $ \s -> s {indents = tail $ indents s}

-- | Succeeds if there is a new line with the same indentation.
nodent :: Parser ()
nodent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- indents <$> getState
  case newIndent == oldIndent of
    True -> return ()
    False -> unexpected "Not a nodent"

-- | Succeeds if there's an empty line, one with only whitespace, or a comment
emptyLine :: Parser ()
emptyLine = try $ newline *> spaces *> finish
  where
    finish = (ignore $ lookAhead $ char '\n') -- <|> ignore pLineComment

-- | Indents, outdents, nodents which also grab up preceding emptylines.
indent', outdent', nodent' :: Parser ()
indent'  = try $ many emptyLine >> indent
outdent' = try $ many emptyLine >> outdent
nodent'  = try $ many emptyLine >> nodent

-- | A semicolon is (mostly) the same as same indentation.
same :: Parser ()
same = nodent' <|> ignore (schar ';')

-- | Parses its argument one or more times, separated by @same@.
blockOf :: Render a => Parser a -> Parser [a]
blockOf p = p `sepEndBy1` same

-- | Parses an indented block of @p@s.
indented :: Render a => Parser a -> Parser [a]
indented p = indent' *> blockOf p <* outdent'

ignore :: Parser a -> Parser ()
ignore p = p >> return ()

perhaps :: Parser a -> Parser ()
perhaps = ignore . optionMaybe

-- | Grabs any character until the stopping parser. Doesn't consume @stop@.
strTill :: Parser a -> Parser Text
strTill stop = fmap pack $ anyChar `manyTill` lookAhead stop

-- | Same as @strTill@, but consumes the stopping parser.
strTillConsume :: Parser a -> Parser Text
strTillConsume stop = fmap pack $ anyChar `manyTill` try stop

-- | For block comments and long strings.
blockStr :: String -> Parser Text
blockStr start = try (string start) >> strTillConsume (string start)

----------------------------------------------------------
----------------------   Debugging   ---------------------
----------------------------------------------------------

-- | Wraps a parser with some logging.
logged :: Render a => Text -> Parser a -> Parser a
logged desc p = do
  debug $ "Attempting to parse '" <> desc <> "'"
  logInput
  ind <- debugIndent <$> getState
  (optionMaybe . try . withIndent) p >>= \case
    Nothing -> do
      debug $ "Failed to parse '" <> desc <> "'"
      unexpected $ unpack $ "Failed to parse '" <> desc <> "'"
    Just a -> do
      debug $ "+++++++++++++++++++++++++++++ " <> desc
              <> " succeeded with `" <> render a <> "`"
      logInput
      return a

-- | Logs the current remaining input.
logInput :: Parser ()
logInput = do input <- getInput
              debug $ "Remaining input: " <> pack (show input)
              level <- head . indents <$> getState
              debug $ "Indentation level: " <> render level

-- | Logs a message. Indents it according to the level.
debug :: Text -> Parser ()
debug msg = do
  ind <- debugIndent <$> getState
  let msg' = T.replicate ind "  " <> msg <> "\n"
  lift $ tell msg'

-- | Wraps a parser, increments/decrements the debug.
withIndent :: Parser a -> Parser a
withIndent p = do
  modifyState $ \s -> s {debugIndent = debugIndent s + 1}
  finally p $ modifyState $ \s -> s {debugIndent = debugIndent s - 1}

-- | Render-prints.
print' :: Render a => a -> IO ()
print' = putStrLn . unpack . pretty

-- | Guarantees that @action@ will be taken even if @parser@ fails.
finally :: Parser a -> Parser b -> Parser a
finally parser action = (try parser <* action) <|> (action *> unexpected "failure")

---------------------------------------------------------
-----------------------  Helpers  -----------------------
---------------------------------------------------------

-- | Parses spaces only. Tabs are not allowed, and newlines are meaningful.
spaces :: Parser ()
spaces = (many $ char ' ') *> return ()

-- | Parses any whitespace, for when we don't care.
anySpaces :: Parser ()
anySpaces = (many $ oneOf " \t\n\r") *> return ()

-- | Parses an item and consumes any following whitespace.
lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

-- | Tries, and if successful then skips spaces.
ltry :: Parser a -> Parser a
ltry = lexeme . try

-- | Wrapper for an abstract expression parser. Gets the current position,
-- runs the parser, and stores the result in an `Expr`.
-- item :: Parser (AbsExpr Expr) -> Parser Expr
-- item parser = Expr <$> getPosition <*> parser

-- | Lifts the intended thing into the context and records the position.
-- item' :: (AbsExpr Expr) -> Parser Expr
-- item' = item . return

-- | Tries, and if successful records the position.
-- itry :: Parser (AbsExpr Expr) -> Parser Expr
-- itry = item . try

-- | String + following whitespace.
sstring :: String -> Parser Text
sstring s = fmap pack (lexeme $ string s)

-- | Same as @sstring@ but for chars.
schar :: Char -> Parser Char
schar = lexeme . char

-- | Wraps common @between@ usage.
enclose :: String -> Parser a -> Parser a
enclose (c1:c2:[]) = between (schar c1) (schar c2)
enclose _ = error "Argument to `enclose` should be a string of length 2"

-- | Parses a given keyword. If it fails, it consumes no input.
pKeyword :: Text -> Parser Text
pKeyword (unpack -> s) = ltry $ pack <$> string s <* notFollowedBy identChar

pAnyKeyword :: Parser Text
pAnyKeyword = choice $ map pKeyword $ toList keywords

-- | Parses the exact symbol given, or consumes nothing.
pExactSym :: String -> Parser Text
pExactSym s = ltry $ pack <$> string s <* notFollowedBy (oneOf symChars)

-- | Fails if the parsed identifier is a keyword.
checkKeyword :: Parser Text -> Parser Text
checkKeyword p = try p >>= \case
  ident | ident `member` keywords -> unexpected $ "keyword " <> show ident
        | otherwise -> return ident

-- | The set of keywords.
keywords :: Set Text
keywords = Data.Set.fromList
  [ "if", "do", "else", "case", "of", "infix", "typedef"
  , "object", "while", "for", "in", "then", "after", "before"
  , "return", "break", "continue", "mut", "ref", "pure"
  , "throw", "try", "catch", "local", "lazy", "with"
  , "block", "forever", "finally"]

-- | Parses any character valid in an identifier.
identChar :: Parser Char
identChar = choice [letter, digit, char '_', char '$']

-- | Gets the position out of an expr.
-- getPos :: Expr -> SourcePos
-- getPos (Expr pos _) = pos

-- | Creates an expression with dummy source position, for testing.
-- toExpr :: AbsExpr Expr -> Expr
-- toExpr = Expr (newPos "" 0 0)

----------------------------------------------------------
-------------------------  Running  ----------------------
----------------------------------------------------------

defaultState :: ParserState
defaultState = ParserState { indents = [0]
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
