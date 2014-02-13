{-# LANGUAGE LambdaCase #-}
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))
import Data.List (intercalate)

type Name = String
data Expr = Id Name
          | Number Double
          | String String
          | Dot Expr Expr
          | Apply Expr Expr
          | Unary String Expr
          | Binary String (Expr, Expr)
          | Tuple [Expr]
          deriving (Eq)

instance Show Expr where
  show expr = case expr of
    Id name -> name
    Number n -> show n
    String s -> show s
    Dot e1 e2 -> show' e1 ++ "." ++ show' e2
    Apply e1 e2 -> show' e1 ++ " " ++ show' e2
    Tuple es -> "(" ++ (intercalate "," . map show) es ++ ")"
    Unary "~" e -> show $ Unary "-" e
    Unary op e -> op ++ show' e
    Binary op (e1, e2) -> show' e1 ++ " " ++ op ++ " " ++ show' e2
    where
      show' expr = case expr of
        Apply _ _ -> "(" ++ show expr ++ ")"
        Dot _ _ -> "(" ++ show expr ++ ")"
        Binary _ _ -> "(" ++ show expr ++ ")"
        Unary _ _ ->  "(" ++ show expr ++ ")"
        _ -> show expr

skip :: Parser ()
skip = many (oneOf " \t") *> return ()

keywords = [ "if", "do", "else", "case", "of", "infix", "type", "object"]
keySyms = ["->", "|", "=", ";", "=>", "?", ":", "#"]

keyword k = lexeme . try $ string k <* notFollowedBy alphaNum

keysym k = lexeme . try $ string k <* notFollowedBy (oneOf symChars)

lexeme p = p <* skip
sstring = lexeme . string
schar = lexeme . char

symChars = "><=+-*/^~!%@&$:.#|?"

check p = lexeme . try $ do
  s <- p
  if s `elem` (keywords ++ keySyms)
    then unexpected $ "reserved word " ++ show s
    else return s

pSymbol :: Parser String
pSymbol = check $ many1 $ oneOf symChars

pId :: Parser Expr
pId = fmap Id $ check $ do
  first <- oneOf $ ['a'..'z'] ++ "_"
  rest <- many $ letter <|> digit <|> char '_'
  return $ first : rest

pDouble :: Parser Double
pDouble = lexeme $ do
  ds <- many1 digit
  option (read ds) $ do
    keysym "."
    ds' <- many1 digit
    return $ read (ds ++ "." ++ ds')

pParens :: Parser Expr
pParens = schar '(' *> expr <* schar ')'
  where expr = pExpr `sepBy` schar ',' >>= \case
                 [e] -> return e
                 es  -> return $ Tuple es

-- data EString = Str String | EString String Expr EString

pString :: Parser String
pString = char '"' >> pString'
pString' = do
  str <- anyChar `manyTill` (lookAhead $ oneOf "\\\"")
  oneOf "\\\"" >>= \case
    '\\' -> anyChar >>= \case
      'n' -> escape '\n'
      '\\' -> escape '\\'
      't' -> escape '\t'
      'r' -> escape '\r'
      'b' -> escape '\b'
      '"' -> escape '"'
      c | c `elem` [' ', '\n', '\t'] -> consume
      c -> unexpected $ "Unrecognized escape character `" ++ [c] ++ "'"
      where escape c = pString' >>= \rest -> return $ str ++ [c] ++ rest
            consume = spaces >> pString' >>= \s -> return (str ++ s)
    '"' -> return str
    c -> error $ "wtf is " ++ [c]


-- pList :: Parser Expr
-- pList = List <$> between (schar '[') (schar ']') get where
--     get = try (do
--       start <- pExpr
--       keysym ".."
--       stop <- pExpr
--       return $ ListRange start stop)
--       <|> ListLiteral <$> (sepBy pExpr (schar ','))

pBinary = pLogical where
  -- | @pBinOp@ takes either @chainl1@ or @chainr1@ for left- or right-
  -- associativity. Takes the higher-precedence parser to try first.
  -- Finally, takes the operator string which if parsed will parse the
  -- expression.
  pBinOp chain higherPrec op = higherPrec `chain` getOp where
    getOp = op >>= \name -> return (\e1 e2 -> Binary name (e1, e2))
  fold first toTry = foldl (pBinOp chainl1) first . map' where
    map' = map (if toTry then try . sstring else sstring)
  pLogical = fold pComp False ["&&", "||"]
  pComp = fold pAdd True ["<=", ">=", "<", ">", "==", "!="]
  pAdd = fold pMult False ["+", "-"]
  pMult = fold pExp False ["*", "/", "%"]
  pExp = pBinOp chainr1 pApply $ sstring "^"

pUnary :: Parser Expr
pUnary = schar '~' *> (Unary "~" <$> pUnary) <|> pTerm

pApply :: Parser Expr
pApply = pDotted >>= parseRest where
  parseRest res = do -- res is a parsed expression
    term <- pDotted -- run the parser again
    parseRest (Apply res term)
    <|> return res -- at some point the second parse will fail; then
                   -- return what we have so far

pDotted :: Parser Expr
pDotted = pUnary >>= parseRest where
  parseRest res = do
    keysym "."
    y <- pTerm
    parseRest (Dot res y)
    <|> return res

pTerm = lexeme $ choice [ Number <$> pDouble, String <$> pString, pId, pParens ]

pExpr :: Parser Expr
pExpr = lexeme $ choice [ pBinary ]

test parser input = parse (skip *> parser) "" input
test' = test pExpr
