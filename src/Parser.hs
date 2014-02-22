{-# LANGUAGE LambdaCase #-}
module Parser ( grab, Expr(..), Statement(..), Block(..)
              , single, ArrayLiteral(..)) where

import Common
import AST
import Text.Parsec hiding (Parser, parse, State)
import Indent
import Control.Applicative hiding (many, (<|>))

-- skip :: Parser ()
-- skip = many (oneOf " \t") *> option () lineComment
--   where lineComment = do char '#'
--                          many (noneOf "\n")
--                          (char '\n' >> return ()) <|> eof
skip = many (oneOf " \t")
keywords = ["if", "do", "else", "case", "of", "infix", "type",
            "object", "while", "for", "in", "then"]
keySyms =  ["->", "|", "=", ";", "..", "=>", "?", ":", "#", "{!"]

keyword k = lexeme . try $ string k <* notFollowedBy alphaNum

exactSym k = lexeme . try $ string k <* notFollowedBy (oneOf symChars)

lexeme p = p <* skip
sstring = lexeme . string
schar = lexeme . char
schar' c = lexeme (char c) >> return ()

check p = lexeme . try $ do
  s <- p
  if s `elem` (keywords ++ keySyms)
    then unexpected $ "reserved word " ++ show s
    else return s

pSymbol :: Parser String
pSymbol = check $ many1 $ oneOf symChars

pIdent :: Parser Char -> Parser String
pIdent firstChar = check $ do
  first <- firstChar
  rest <- many $ alphaNum <|> char '_'
  bangs <- many $ char '!' <|> char '\''
  return $ first : rest ++ bangs

pVar :: Parser Expr
pVar = do
  var <- Var <$> pIdent (lower <|> char '_')
  option var $ do
    typ <- exactSym ":" *> pType
    return $ Typed var typ

pConstructor = Constructor <$> pIdent upper

pDouble :: Parser Double
pDouble = lexeme $ do
  ds <- many1 digit
  option (read ds) $ do
    exactSym "."
    ds' <- many1 digit
    return $ read (ds ++ "." ++ ds')

pType :: Parser Type
pType = choice [pTApply, pTTuple]
pTVar = TVar Rigid <$> many1 lower
pTConst = (\n -> TConst n []) <$> pIdent upper
pTParens = schar '(' *> sepBy pType (schar ',') <* schar ')'
pTTuple = tTuple <$> pTParens
pTApply = pIdent upper >>= getArgs where
  getArgs name =
    TConst name [] `option` do
      args <- choice [ single <$> (pTVar <|> pTConst), pTParens ]
      return $ TConst name args

pTypedVar :: Parser Expr
pTypedVar = try $ Typed <$$ pVar <* exactSym ":" <*> pType

pLambda = try $ do
  argsBodies <- pArgBody `sepBy1` (exactSym "|")
  case argsBodies of
    [(arg, body)] -> return $ Lambda arg body
    _ -> do
      name <- unusedName
      return $ Lambda (Var name) $ Case (Var name) argsBodies

unusedName :: Parser Name
unusedName = return "(arg)"

pArgBody = (,) <$$ pTerm <* exactSym "=>" <*> pExprOrBlock

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
      exactSym ".."
      stop <- pExpr
      return $ ArrayRange start stop)
      <|> ArrayLiteral <$> (sepBy pExpr (schar ','))

pBinary :: Parser Expr
pBinary = pBackwardApply where
  -- | @pBinOp@ takes either @chainl1@ or @chainr1@ for left- or right-
  -- associativity. Takes the higher-precedence parser to try first.
  -- Finally, takes the operator string which if parsed will parse the
  -- expression.
  pBinOp chain higherPrec op = higherPrec `chain` getOp where
    getOp = op >>= \name -> return (\e1 e2 -> binary name e1 e2)
  fold first = foldl (pBinOp chainl1) first . map exactSym
  pBackwardApply = pBinOp chainr1 pForwardApply (exactSym "<|")
  pForwardApply = pBinOp chainl1 pLogical (exactSym "|>")
  pLogical = fold pComp ["&&", "||"]
  pComp = fold pAdd ["<=", ">=", "<", ">", "==", "!="]
  pAdd = fold pMult ["+", "-"]
  pMult = fold pExp ["*", "/", "%"]
  pExp = pBinOp chainr1 pComposeRight $ sstring "^"
  pComposeRight = pBinOp chainl1 pComposeLeft (exactSym "~>")
  pComposeLeft = pBinOp chainr1 pApply (exactSym "<~")

pUnary :: Parser Expr
pUnary = exactSym "~" *> (Apply (Var "~") <$> pUnary) <|> pTerm

pApply :: Parser Expr
pApply = pRef >>= parseRest where
  parseRest res = do -- res is a parsed expression
    term <- pRef -- run the parser again
    parseRest (Apply res term)
    <|> return res -- at some point the second parse will fail; then
                   -- return what we have so far

pRef :: Parser Expr
pRef = pDotted >>= parseRest where
  parseRest res = do
    y <- exactSym "{!" *> pExpr <* pCloseBrace
    parseRest (Ref res y)
    <|> return res

pDotted :: Parser Expr
pDotted = pUnary >>= parseRest where
  parseRest res = do
    exactSym "."
    y <- pUnary
    parseRest (Dot res y)
    <|> return res

pTerm :: Parser Expr
pTerm = lexeme $ choice [ Number <$> pDouble
                        , String <$> pString
                        , pVar
                        , pConstructor
                        , pParens
                        , pArray ]

pOpenBrace = schar'  '{' >> notFollowedBy (char '!')
pCloseBrace = schar'  '}'

pBlock = pOpenBrace *> pStatementsNoWS <* pCloseBrace
         <|> indent *> pStatementsWS <* dedent
         <|> (keyword "do" >> single <$> pStatement)

pWhile = While <$ keyword "while" <*> pExpr <*> pBlock
pFor = For <$ keyword "for" <*> pExpr <* keyword "in"
           <*> pExpr <*> pBlock

pExprOrBlock :: Parser Expr
pExprOrBlock = try (Block <$> pBlock) <|> pExpr

pBody = try pBlock <|> (single <$> Expr <$> pExpr)

pDefine = choice [pDefineBinary, pDefineFunction]

pDefineFunction = try $ do
  name <- pIdent lower
  args <- many pTerm
  body <- exactSym "=" *> pExprOrBlock
  return $ Define name $ foldr Lambda body args

pDefineBinary = try $ do
  arg1 <- pTerm
  op   <- pSymbol
  arg2 <- pTerm
  body <- exactSym "=" *> pExprOrBlock
  return $ Define op $ Lambda (Tuple [arg1, arg2]) body

pAssign = try $ Assign <$$ pExpr <* exactSym ":=" <*> pExprOrBlock

getSame = same
pStatementsNoWS = pStatement `sepEndBy1` (schar ';')
pStatementsWS = pStatement `sepEndBy1` getSame
pStatements = pStatement `sepEndBy1` (getSame <|> schar'  ';')

pIf = do
  keyword "if"
  cond <- pExpr
  true <- pThen
  false <- pElse
  return $ If cond true false

pBlockOrStatement = try pBlock <|> fmap single pStatement
pThen = do keyword "then" <|> return "then"
           pBlockOrStatement
pElse = keyword "else" *> pBlockOrStatement

pStatement :: Parser Statement
pStatement = do
  lexeme $ choice $ [ pDefine, pAssign
                    , Expr <$> pExpr
                    , pWhile, pFor
                    , pIf]

pExpr :: Parser Expr
pExpr = lexeme $ choice [ pLambda, pBinary ]

single = pure

testE = parse pExpr
testS = parse pStatementsWS

grab :: String -> Either ParseError [Statement]
grab = parse (pStatements <* eof)

grab' input = case grab input of
  Right statements -> map show statements ! intercalate "\n"
  Left err -> error $ show err
