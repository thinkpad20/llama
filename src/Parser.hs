{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
module Parser where

import System.IO.Unsafe
import Text.Parsec hiding (Parser, parse, State)
import Control.Applicative hiding (many, (<|>))
import Data.List (intercalate)
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.State

type ParseState = Int
type Source = String
type Parser = ParsecT Source ParseState IO
type Name = String
data Expr = Id Name
          | Number Double
          | String String
          | Dot Expr Expr
          | Apply Expr Expr
          | Unary String Expr
          | Binary String (Expr, Expr)
          | Tuple [Expr]
          | Array ArrayLiteral
          deriving (Eq)

data ArrayLiteral = ArrayLiteral [Expr] | ArrayRange Expr Expr deriving (Eq)

type Block = [Statement]
data Statement = Expr Expr
               | If Expr Block Block
               | If' Expr Block
               | While Expr Block
               | For Expr Expr Block
               | Define Expr Block
               | Assign Expr Block
               | Return Expr
               | Throw Expr
               | Break
               deriving (Eq)

instance Show Statement where
  show stmt = fst $ runState (render stmt) 0 where
    render :: Statement -> State Int String
    render stmt = case stmt of
      Expr expr -> line $ show expr
      If c t f -> do
        if' <- line $ "if " ++ show c
        else' <- line "else"
        t' <- block t
        f' <- block f
        join $ [if'] ++  t' ++ [else'] ++ f'
      If' c t -> do
        if' <- line $ "if " ++ show c
        t' <- block t
        join $ [if'] ++ t'
      While e blk -> do
        while <- line $ "while " ++ show e
        blk' <- block blk
        join $ [while] ++ blk'
      For pat expr blk -> do
        for <- line $ "for " ++ show pat ++ " in " ++ show expr
        blk' <- block blk
        join $ [for] ++ blk'
      Define e1 [Expr e2] -> line $ show e1 ++ " = " ++ show e2
      Define e blk -> do
        expr <- line $ show e ++ " ="
        body <- block blk
        join $ [expr] ++ body
      Assign e1 block -> line $ show e1 ++ " := " ++ show block
      Break -> line "break"
      Throw e -> line $ "throw " ++ show e
      Return e -> line $ "return " ++ show e
    line str = get >>= \i -> return $ replicate i ' ' ++ str
    join = return . intercalate "\n"
    block blk = up *> mapM render blk <* down
    (up, down) = (modify (+2), modify (\n -> n - 2))


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


-- skip :: Parser ()
-- skip = many (oneOf " \t") *> option () lineComment
--   where lineComment = do char '#'
--                          many (noneOf "\n")
--                          (char '\n' >> return ()) <|> eof
skip = many (oneOf " \t")
keywords = ["if", "do", "else", "case", "of", "infix", "type",
            "object", "while", "for"]
keySyms =  ["->", "|", "=", ";", "..", "=>", "?", ":", "#"]

keyword k = lexeme . try $ string k <* notFollowedBy alphaNum

keysym k = lexeme . try $ string k <* notFollowedBy (oneOf symChars)

lexeme p = p <* skip
sstring = lexeme . string
schar = lexeme . char
schar' c = lexeme (char c) >> return ()

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
  bangs <- many $ char '!'
  return $ first : rest ++ bangs

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

pArray :: Parser Expr
pArray = Array <$> between (schar '[') (schar ']') get where
    get = try (do
      start <- pExpr
      keysym ".."
      stop <- pExpr
      return $ ArrayRange start stop)
      <|> ArrayLiteral <$> (sepBy pExpr (schar ','))

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

pIndent, pDedent :: Parser ()
(pIndent, pDedent) = (pDent "indent" (>), pDent "dedent" (<)) where
  pDent name comp = try $ do
    many $ char '\n'
    spaces <- length <$> many (char ' ')
    level  <- getLevel
    prnt $ "counted " ++ show spaces ++ " spaces"
    if spaces `comp` level
    then do
      prnt $ "level went from " ++ show level ++ " to " ++ show spaces
      setLevel spaces
    else do
      prnt $ "Tried to " ++ name ++ " but we got " ++ show spaces ++ " and level was "  ++ show level
      unexpected "Different indentation"

getLevel = getState
setLevel = setState

pEndLine = char '\n' <|> schar ';'
prnt = lift . putStrLn
single = pure
pBlock = schar' '{' *> pStatements <* schar' '}'
         <|> pIndent *> pStatements <* pDedent
         <|> (keyword "do" >> single <$> pStatement)

pWhile = do keyword "while"
            cond <- pExpr
            block <- pBlock
            return $ While cond block

pBody = try pBlock <|> (single <$> Expr <$> pExpr)
pDefine = try $ pure Define <*> pExpr <* keysym "=" <*> pBody
pAssign = try $ pure Assign <*> pExpr <* keysym ":=" <*> pBody

pStatements = pStatement `sepEndBy1` pEndLine
pStatement = do
  level <- getLevel
  prnt $ "Parsing a statement, level is " ++ show level
  lexeme $ choice $ [pDefine, pAssign, Expr <$> pExpr, pWhile]

pExpr :: Parser Expr
pExpr = lexeme $ choice [ pBinary ]

parse :: Parser a -> ParseState -> Source -> IO (Either ParseError a)
parse p u s = runParserT p u "" s

grab input = case test' pStatements input of
  Left err -> error $ show err
  Right stmts -> stmts

test' parser input = unsafePerformIO $ parse (skip *> parser) 0 input
test input = (intercalate "\n" . map show) <$> test' pStatements input
ptest input = case test input of
  Left err -> print err
  Right val -> putStrLn val
