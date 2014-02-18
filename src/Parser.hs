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
            "object", "while", "for", "in"]
keySyms =  ["->", "|", "=", ";", "..", "=>", "?", ":", "#", "{!"]

keyword k = lexeme . try $ string k <* notFollowedBy alphaNum

keysym k = lexeme . try $ string k <* notFollowedBy (oneOf symChars)

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
  var <- Var <$> pIdent (oneOf $ ['a'..'z'] ++ "_")
  option var $ do
    typ <- keysym ":" *> pType
    return $ Typed var typ

pConstructor = Constructor <$> pIdent upper

pDouble :: Parser Double
pDouble = lexeme $ do
  ds <- many1 digit
  option (read ds) $ do
    keysym "."
    ds' <- many1 digit
    return $ read (ds ++ "." ++ ds')

pType :: Parser Type
pType = pTTerm
pTTerm = choice [pTVar, pTConst, pTTuple]
pTVar = TVar Rigid <$> many1 lower
pTParens = schar '(' *> sepBy pType (schar ',') <* schar ')'
pTConst = do name <- pIdent upper
             TConst name <$> get
  where get = (pure <$> pTVar) <|> pTParens <|> return []
pTTuple = tTuple <$> pTParens

pTypedVar :: Parser Expr
pTypedVar = try $ Typed <$$ pVar <* keysym ":" <*> pType

pLambda = try $ do
  argsBodies <- pArgBody `sepBy1` (keysym "|")
  return $ Lambda argsBodies

pArgBody = (,) <$$ pTerm <* keysym "=>" <*> pBody

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

pBinary :: Parser Expr
pBinary = pBackwardApply where
  -- | @pBinOp@ takes either @chainl1@ or @chainr1@ for left- or right-
  -- associativity. Takes the higher-precedence parser to try first.
  -- Finally, takes the operator string which if parsed will parse the
  -- expression.
  pBinOp chain higherPrec op = higherPrec `chain` getOp where
    getOp = op >>= \name -> return (\e1 e2 -> binary name e1 e2)
  fold first toTry = foldl (pBinOp chainl1) first . map' where
    map' = map keysym -- (if toTry then try . sstring else sstring)
  pBackwardApply = pBinOp chainr1 pForwardApply (keysym "<|")
  pForwardApply = pBinOp chainl1 pLogical (keysym "|>")
  pLogical = fold pComp True ["&&", "||"]
  pComp = fold pAdd True ["<=", ">=", "<", ">", "==", "!="]
  pAdd = fold pMult False ["+", "-"]
  pMult = fold pExp False ["*", "/", "%"]
  pExp = pBinOp chainr1 pComposeRight $ sstring "^"
  pComposeRight = pBinOp chainl1 pComposeLeft (keysym "~>")
  pComposeLeft = pBinOp chainr1 pApply (keysym "<~")

pUnary :: Parser Expr
pUnary = keysym "~" *> (Apply (Var "~") <$> pUnary) <|> pTerm

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
    y <- keysym "{!" *> pExpr <* pCloseBrace
    parseRest (Ref res y)
    <|> return res

pDotted :: Parser Expr
pDotted = pUnary >>= parseRest where
  parseRest res = do
    keysym "."
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

pBody = try pBlock <|> (single <$> Expr <$> pExpr)
pDefine = try $ do
  var <- pExpr
  body <- keysym "=" *> pBody
  create var body
  where
    create var body = case var of
      Var _ -> return $ Define var body
      Typed (Var _) _ -> return $ Define var body
      Apply name@(Var _) arg -> mkLambda name arg body
      Apply name@(Typed _ _) arg -> mkLambda name arg body
      otherwise -> unexpected $ "Illegal definition of `" ++ show var ++ "`"
    mkLambda name arg body = return $
      Define name $ [Expr $ Lambda [(arg, body)]]

pAssign = try $ Assign <$$ pExpr <* keysym ":=" <*> pBody

getSame = same
pStatementsNoWS = pStatement `sepEndBy1` (schar ';')
pStatementsWS = pStatement `sepEndBy1` getSame
pStatements = pStatement `sepEndBy1` (getSame <|> schar' ';')

pIf = If <$ keyword "if" <*> pExpr <*> pThen <*> pElse
pThen = pBlock <|> keyword "then" *> (single <$> pStatement)
pElse = keyword "else" *> pBlock

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
