{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Llama.Parser.Tokenizer (tokenize, PToken(..)) where

import qualified Prelude as P
import Prelude (Show)
import Data.Set hiding (map)
import Text.Parsec hiding (many, (<|>), spaces)
import qualified Data.Text as T
import System.IO.Unsafe

import Language.Llama.Common.Common hiding (fromList, member)
import Language.Llama.Parser.Tokens

data PToken = PToken
  { tPosition :: SourcePos
  , tHasSpace :: Bool
  , tToken    :: Token PToken} deriving (Show, Eq)

instance Render PToken where
  render pt = do
    let (pos, tok) = (tPosition pt, tToken pt)
        sps = if tHasSpace pt then "" else " (no spaces)"
    render pos <> " " <> render tok <> sps

data TokenizerState = TokenizerState
  { tsIndentLevels :: [Int]
  , tsHasSpace     :: Bool } deriving (Show, Eq)

type TokenizerError = ParseError

type Tokenizer = ParsecT String TokenizerState IO

---------------------------------------------------------
-----------------------  Helpers  -----------------------
---------------------------------------------------------

-- | Grabs 0 or more spaces (not tabs). If at least one, sets tsHasSpace.
spaces :: Tokenizer String
spaces = many (char ' ') >>== \case
  [] -> return ()
  _ -> modifyState $ \s -> s {tsHasSpace=True}

-- | Similar to @spaces@, but will grab any whitespace.
whitespace :: Tokenizer String
whitespace = many (oneOf "\r\n\t ") >>== \case
  [] -> return ()
  _ -> modifyState $ \s -> s {tsHasSpace=True}

-- | Grabs the given string and any trailing spaces.
sstring :: String -> Tokenizer String
sstring = lexeme . string

-- | Grabs a token and any trailing spaces.
lexeme :: Tokenizer a -> Tokenizer a
lexeme tizer = tizer <* spaces

-- | Wrapper that adds metadata to a token.
item :: Tokenizer (Token PToken) -> Tokenizer PToken
item tokenizer = do
  pos <- getPosition
  sps <- tsHasSpace <$> getState
  -- Reset the spaces.
  modifyState $ \s -> s {tsHasSpace=False}
  token <- tokenizer
  return PToken {
    tHasSpace = sps
  , tToken = token
  , tPosition = pos
  }

-- | @manyTill@ is often used with lookAhead- or try-based tokenizers.
manyTillLook, manyTillTry :: Tokenizer a -> Tokenizer b -> Tokenizer [a]
manyTillLook p1 p2 = p1 `manyTill` lookAhead p2
manyTillTry p1 p2 = p1 `manyTill` try p2

-- | Grabs any character until the stopping parser. Doesn't consume @stop@.
strTill :: Tokenizer a -> Tokenizer Text
strTill stop = fmap pack $ anyChar `manyTillLook` stop

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

-- | Characters valid to be found in an identifier.
identChars :: String
identChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_@$"

-- | An identifier.
identifier :: Tokenizer Name
identifier = do
  first <- letter <|> (char '_' <* notFollowedBy (oneOf symChars))
  rest <- many $ oneOf identChars
  return $ pack (first : rest)

-- | Same as @identifier@ but wraps in a token.
tAnyId :: Tokenizer (Token a)
tAnyId = TId <$> identifier

-- | Same as @tAnyId@ but fails if it was a keyword.
tId :: Tokenizer (Token a)
tId = try $ do
  id <- identifier
  if id `elem` keywords then unexpected $ "keyword: " <> unpack id
  else return $ TId id

-- | A numeric constant (either Int or Float).
tNum :: Tokenizer (Token a)
tNum = do
  first <- many1 digit
  option (TInt $ read first) $ do
    dot <- try (char '.' <* notFollowedBy (char '.' <|> oneOf identChars))
    rest <- many1 digit
    return $ TFloat $ read $ first <> (dot : rest)

-- | A keyword.
tKeyword :: Tokenizer (Token a)
tKeyword = try $ choice $ map go keywords
  where
    go name = try $ do
      string (unpack name)
      notFollowedBy $ oneOf identChars
      return $ TKeyword name

------------------------------------------------------------
-------------------  Strings & regexes  --------------------
------------------------------------------------------------

-- | A "regular" string, i.e. one without interpolation. Uses single quotes.
tRegString :: Tokenizer (Token a)
tRegString = char '\'' >> TStr <$> go where
  go = do
    str <- strTill (oneOf "\\'")
    let escape c = ((str `snoc` c) <>) <$> go
    oneOf "\\'" >>= \case
      '\\' -> anyChar >>= \case
        'n'  -> escape '\n'
        '\\' -> escape '\\'
        't'  -> escape '\t'
        'r'  -> escape '\r'
        'b'  -> escape '\b'
        c | c `elem` [' ', '\n', '\t'] -> consume
          | otherwise -> escape c
        where consume = whitespace >> (str <>) <$> go
      '\'' -> return str
      c -> error $ "wtf is " <> [c]

-- | An interpolated string. Uses double quotes.
tInString :: Tokenizer (Token PToken)
tInString = char '"' >> TIStr <$> go where
  go = do
    str <- Plain . pack <$> many (noneOf "\"#\\")
    anyChar >>= \case
      '\\' -> do
        c <- anyChar
        fmap ((str <> Plain ("\\" <> T.singleton c)) <>) $ go
      '#' -> anyChar >>= \case
        '{' -> do
           str' <- balancedString '}'
           pos <- getPosition
           case tokenizeFrom pos str' of
            Left err -> error $ "Error in interpolated string: " <> P.show err
            Right (tokens, pos') -> do
              setPosition pos'
              IStr str tokens <$> go
      '"' -> return str

-- | Balances quotes and curly braces, so that we can grab strings within
-- interpolated strings.
balancedString :: Char -> Tokenizer String
balancedString stop = P.reverse <$> loop ([stop], "")
  where
    loop :: ([Char], String) -> Tokenizer String
    loop (stop:rest, str) = anyChar >>= \case
      c | c == stop -> case rest of [] -> return str
                                    _  -> loop (rest, c:str)
      '\\' -> anyChar >>= \c -> loop (stop:rest, c:'\\':str)
      '#' | stop == '"' -> anyChar >>= \case
        '{' -> loop ('}':stop:rest, '{':'#':str)
        c   -> loop (stop:rest, c:'#':str)
      c | stop == '}' && c `elem` ['"', '\''] -> loop (c:stop:rest, c:str)
      c -> loop (stop:rest, c:str)

---------------------------------------------------------
-----------------------  Indentation  -------------------
---------------------------------------------------------

-- | An indent. Won't consume input if it's not an indent.
tIndent :: Tokenizer (Token a)
tIndent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- tsIndentLevels <$> getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent *> return Indent
    False -> unexpected "Not an indent"
  where pushIndent i = modifyState $ push i
        push i s = s {tsIndentLevels = i:tsIndentLevels s}

-- | An outdent. If it succeeds, it pops an indentation level off the stack,
-- but it doesn't consume input. So it can be run multiple times in a row,
-- if there are multiple outdents. EOF counts as an outdent.
tOutdent :: Tokenizer (Token a)
tOutdent = try $ do
  isOutdent <- lookAhead $ (eof >> return True) <|> (
    do newline
       newIndent <- length <$> many (char ' ')
       oldIndent:_ <- tsIndentLevels <$> getState
       return (newIndent < oldIndent))
  case isOutdent of
    True -> popIndent *> return Outdent
    False -> unexpected "Not an outdent"
  where
    popIndent = modifyState pop
    pop s = s {tsIndentLevels = tail $ tsIndentLevels s}

-- | A nodent is a newline character followed by the SAME level of indentation
-- that the previous line had.
tNodent :: Tokenizer (Token a)
tNodent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- tsIndentLevels <$> getState
  case newIndent == oldIndent of
    True -> return Nodent
    False -> unexpected "Not a nodent"

-- | Grabs any empty lines, meaning lines consisting only of spaces, or spaces
-- followed by a line/block comment.
tEmptyLine :: Tokenizer String
tEmptyLine = try $ char '\n' >> char ' ' `manyTillLook` char '\n'

-- | Any of the indents, which any preceding empty lines skipped.
tDent :: Tokenizer (Token a)
tDent = try $ many tEmptyLine >> (tIndent <|> tNodent <|> tOutdent)

------------------------------------------------------------
----------------------- Punctuation  -----------------------
------------------------------------------------------------

-- | All characters valid to use in symbols.
symChars :: String
symChars = "+*-/|&><=?!~:_"

-- | Grabs a symbol.
tSymbol :: Tokenizer (Token a)
tSymbol = try $ do
  sym <- many1 $ oneOf symChars
  if none sym then return $ TSymbol $ pack sym
  else if isIdent sym then return $ TId $ pack sym
  else unexpected $ "Invalid symbol: " <> sym
  where
    u = (== '_')
    none = not . any u
    isPre s = u (P.last s) && none (P.init s)
    isPost s = u (head s) && none (P.tail s)
    isInfix s = u (P.last s) && u (head s) && none (P.tail $ P.init s)
    isIdent s = isInfix s || isPre s || isPost s

-- | Grabs a specific symbol
tThisSymbol :: String -> Tokenizer (Token a)
tThisSymbol sym = try (string sym) >> return (TSymbol $ pack sym)

-- Grabs some "punctuation", such as parentheses, semicolons, etc.
tPunc :: Char -> Tokenizer (Token a)
tPunc = char >=> return . TPunc

-- | Takes a list of characters and tries to make a @Punc@ out of one of them.
tCharPuncs :: [Char] -> Tokenizer (Token a)
tCharPuncs = choice . map tPunc

------------------------------------------------------------
------------------------  Comments  ------------------------
------------------------------------------------------------

tLineComment :: Tokenizer (Token a)
tLineComment = do
  char '#'
  body <- anyChar `manyTillLook` char '\n'
  pure $ TLineComment $ pack body

tBlockComment :: Tokenizer (Token a)
tBlockComment = do
  try $ string "###"
  body <- anyChar `manyTillTry` string "###"
  pure $ TBlockComment $ pack body

------------------------------------------------------------
-------------------  High-level parsers  -------------------
------------------------------------------------------------

-- | Grabs a list of tokens. Appends @Outdent@ tokens to the end if there
-- are leftover indentations.
tTokens :: Tokenizer [PToken]
tTokens = do
  tokens <- tOneToken `manyTill` eof
  indents <- tsIndentLevels <$> getState
  pos <- getPosition
  let outdent = PToken pos False Outdent
      go acc [0] = return $ tokens <> P.reverse acc
      go acc (i:is) = go (outdent:acc) is
  go [] indents

tOneToken :: Tokenizer PToken
tOneToken = lexeme $ item $ choice
  [ tBlockComment
  , tLineComment
  , tRegString
  , tInString
  , tKeyword
  , tId
  , tNum
  , tThisSymbol ".="
  , tThisSymbol "!"
  , tSymbol
  , tThisSymbol ".."
  , tPunc '.'
  , tDent
  , tCharPuncs "(){}[]:,;\\"
  ]

----------------------------------------------------------
-----------------  Running the tokenizer -----------------
----------------------------------------------------------

-- | The default initial state of the tokenizer.
initState :: TokenizerState
initState = TokenizerState
  { tsIndentLevels = [0]
  , tsHasSpace = True }

--
tokenize :: String -> Either TokenizerError [PToken]
tokenize = runTokens tTokens

runTokens :: Tokenizer a -> String -> Either TokenizerError a
runTokens tizer input = do
  let a = runParserT tizer initState "" input
  unsafePerformIO a

-- | Tokenizes from a given starting position. Returns the ending position.
tokenizeFrom :: SourcePos
             -> String
             -> Either TokenizerError ([PToken], SourcePos)
tokenizeFrom pos = runTokensFrom pos tTokens

-- | Runs a tokenizer given a starting source position. Is used when we
-- tokenize a string inside of a string interpolation.
runTokensFrom :: SourcePos   -- ^ Position from which to start
              -> Tokenizer a -- ^ The tokenizer to run
              -> String      -- ^ The input string
              -> Either TokenizerError (a, SourcePos) -- ^ Result and new pos
runTokensFrom pos tizer input = do
  let tizer' = do
        setPosition pos
        res <- tizer
        pos' <- getPosition
        return (res, pos')
  unsafePerformIO $ runParserT tizer' initState "" input


puts :: String -> Tokenizer ()
puts = liftIO . putStrLn

putt :: Text -> Tokenizer ()
putt = liftIO . putStrLn . unpack

testFile :: FilePath -> IO ()
testFile = readFile >=> test

test :: String -> IO ()
test s = case tokenize s of
  Right tokens -> mapM_ print tokens
  Left err -> error $ P.show err
