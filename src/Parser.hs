{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser ( grab
              , Expr(..)
              , Block
              , grabT
              , grabOrError) where

import Text.Parsec hiding (Parser, parse, State)
import Control.Applicative hiding (many, (<|>))
import qualified Data.Text as T
import qualified Data.Map as M

import Common
import AST
import TypeLib
import ParserLib

instance Render ParseError

skip :: Parser ()
skip = many (oneOf " \t") *> option () lineComment
  where lineComment = do char '#'
                         many (noneOf "\n")
                         (char '\n' >> return ()) <|> eof
keywords = [ "if", "do", "else", "case", "of", "infix", "typedef"
           , "object", "while", "for", "in", "then", "after", "before"
           , "return", "break", "continue", "mut", "ref", "pure"
           , "local", "lazy", "extends", "with", "block"]
keySyms =  ["->", "|", "=", ";", "..", "=>", "?", ":", "#", ":=", "&="]

keyword k = fmap T.pack $ go where
  go = lexeme . try $ string (T.unpack k) <* notFollowedBy (alphaNum <|> char '_')

exactStr = keyword


exactSym k = fmap T.pack go where
  go = lexeme . try $ string k <* notFollowedBy (oneOf (symChars <> "."))

lexeme p = p <* skip
sstring s = fmap T.pack (lexeme $ string s)
schar c = lexeme $ char c
schar' c = lexeme (char c) >> return ()

check p = lexeme . try $ do
  s <- p
  if s `elem` (keywords ++ keySyms)
    then unexpected $ "reserved word " ++ show s
    else return s

lowers :: Parser String
lowers = check $ many1 lower

pSymbol :: Parser T.Text
pSymbol = fmap T.pack $ check $ many1 $ oneOf symChars <* notFollowedBy (char '_')

pIdent :: Parser Char -> Parser T.Text
pIdent firstChar = fmap T.pack $ check $ do
  first <- firstChar
  rest <- many $ alphaNum <|> char '_' <|> char '-' <|> char '\''
  bangs <- many $ char '!'
  case (first : rest ++ bangs) of
    "_" -> unexpected "Single underscore"
    ident | last ident == '-' -> unexpected "Ends in a dash"
          | otherwise -> return ident

pVar :: Parser Expr
pVar = do
  var <- getVar
  option var $ do
    typ <- exactSym ":" *> pType
    return $ Typed var typ
  where getVar = choice $ map (fmap Var . try) ps
        chk s = if s `elem` keySyms
          then unexpected $ "reserved symbol " ++ show s
          else return s
        prefix = do
          s <- chk =<< many1 (oneOf symChars)
          c <- char '_'
          return $ T.pack $ s ++ [c]
        postfix = do
          c <- char '_'
          s <- chk =<< many1 (oneOf symChars)
          return $ T.pack $ c : s
        infix_ = do
          s <- char '_' *> (chk =<< many1 (oneOf symChars)) <* char '_'
          return $ T.pack s
        ps =  [ pIdent (lower <|> char '_')
              , infix_, prefix, postfix]

pConstructor = Constructor <$> pIdent upper

pDouble :: Parser Double
pDouble = lexeme $ do
  ds <- many1 digit
  option (read ds) $ getDecimal ds
  where getDecimal ds = try $ do exactSym "."
                                 ds' <- many1 digit
                                 return $ read (ds ++ "." ++ ds')


pObjectDec :: Parser ObjectDec
pObjectDec = do
  keyword "object"
  name <- pIdent upper
  vars <- many $ fmap T.pack lowers
  extends <- optionMaybe (keyword "extends" *> pIdent upper)
  exactSym "="
  (constrs, attrs) <- getConstrs
  return $ defObj {
      objName = name
    , objExtends = extends
    , objVars = vars
    , objConstrs = constrs
    , objAttrs = attrs
    }

getConstrs = do
  schar '{'
  constrs <- pConstructorDec `sepBy1` schar ';'
  attrs <- option [] $ do
    keyword "with"
    pTerm `sepBy1` schar ';'
  schar '}'
  return (constrs, attrs)

pConstructorDec :: Parser ConstructorDec
pConstructorDec = do
  name <- pIdent upper
  args <- many pTerm
  extends <- optionMaybe (keyword "extends" >> pExpr)
  logic <- optionMaybe pExprOrBlock
  return $ defConstr {
      constrName = name
    , constrArgs = args
    , constrExtends = extends
    , constrLogic = logic
    }

pType :: Parser Type
pType = choice [pTFunction]
pTTerm = choice [pMultiFunc, pTVector, pTList, pTMap, pTSet, pTVar, pTConst, pTTuple]
pTVar = TVar <$> fmap T.pack lowers <* skip
pTConst = TConst <$> pIdent upper
pTParens = schar '(' *> sepBy pType (schar ',') <* schar ')'
pTTuple = pTParens >>= \case
  [t] -> return t
  ts -> return $ tTuple ts
pTFunction = chainr1 pTApply (exactSym "->" *> pure TFunction)
pTApply = chainl1 pTTerm (pure TApply)
pTVector = fmap arrayOf (exactSym "[" *> pType <* schar ']')
pTList = fmap listOf (sstring "[!" *> pType <* schar ']')
pTSet = fmap setOf (exactSym "{" *> pType <* schar '}')
pTMap = fmap mapOf $ try $ do
  exactSym "{"
  t1 <- pType
  exactSym "=>"
  t2 <- pType
  schar '}'
  return (t1, t2)
pMultiFunc = do
  keyword "{m"
  fromTos <- sepBy1 pFromTo (schar ',')
  schar '}'
  return $ TMultiFunc $ M.fromList fromTos
  where
    pFromTo = pTFunction >>= \case
      TFunction from to -> return (from, to)
      _ -> unexpected "Didn't parse a function type in MultiFunction."

pTypeDef :: Parser Expr
pTypeDef =
  TypeDef <$ keyword "typedef" <*> pIdent upper <* exactSym "=" <*> pType

pParens :: Parser Expr
pParens = schar '(' *> exprs <* schar ')'
  where exprs = exprsAndKwargs >>= \case
          Tuple [e] k | k == mempty -> return e
          tup -> return tup
        getExprs = try pExpr `sepEndBy` schar ','
        exprsOnly = tuple <$> getExprs
        kwargsOnly = Tuple [] <$> kwargs
        exprsAndKwargs = Tuple <$> getExprs <*> kwargs
        kwarg = do
          name <- char '@' *> pIdent lower
          x <- choice [ fmap Left $ exactSym "=" *> pExpr
                      , fmap Right $ exactSym ":" *> pType ]
          return (name, x)
        kwargs = kwarg `sepBy` schar ','
        expr = choice [pExpr, Var <$> pSymbol]

pString, pString' :: Parser T.Text
pString = char '"' >> pString'
pString' = do
  str <- fmap T.pack (anyChar `manyTill` (lookAhead $ oneOf "\\\""))
  oneOf "\\\"" >>= \case
    '\\' -> anyChar >>= \case
      'n'  -> escape '\n'
      '\\' -> escape '\\'
      't'  -> escape '\t'
      'r'  -> escape '\r'
      'b'  -> escape '\b'
      '"'  -> escape '"'
      c | c `elem` [' ', '\n', '\t'] -> consume
      c -> unexpected $ "Unrecognized escape character `" ++ [c] ++ "'"
      where escape c = pString' >>= \rest -> return $ str <> T.singleton c <> rest
            consume = spaces >> pString' >>= \s -> return (str <> s)
    '"' -> return str
    c -> error $ "wtf is " ++ [c]

pLiteral :: Parser Expr
pLiteral = Literal <$> choice [try array, list, try set, dict] where
  commas p = p `sepBy` schar ','
  array = between (schar '[' <* notFollowedBy (char 'l')) (schar ']') get where
    get = try (do
      start <- pExpr
      exactSym ".."
      stop <- pExpr
      return $ ArrayRange start stop)
      <|> ArrayLiteral <$> commas pExpr
  list = between (exactSym "[l") (schar ']') (ListLiteral <$> commas pExpr)
  set = between (exactSym "{s") (schar '}') (SetLiteral <$> commas pExpr)
  rocket = (,) <$> pTerm <* exactSym "=>" <*> pExpr
  dict = between (exactSym "{d") (schar '}')
                 (DictLiteral <$> commas rocket)

pBinary :: Parser Expr
pBinary = pBackwardApply where
  -- | @pBinOp@ takes either @chainl1@ or @chainr1@ for left- or right-
  -- associativity. Takes the higher-precedence parser to try first.
  -- Finally, takes the operator string which if parsed will parse the
  -- expression.
  pBinOp chain higherPrec op = higherPrec `chain` getOp where
    getOp = op >>= \name -> return (\e1 e2 -> binary name e1 e2)
  fold first = foldl (pBinOp chainl1) first . map exactSym
  fold' first = foldl (pBinOp chainr1) first . map exactSym
  pBackwardApply = pBinOp chainr1 pForwardApply (exactSym "$")
  pForwardApply = pBinOp chainl1 pLogical (exactSym "|>")
  pLogical = fold' pComp ["&&", "||"]
  pComp = fold pAdd ["<=", ">=", "<", ">", "==", "!="]
  pAdd = fold pMult ["+", "-"]
  pMult = fold pExp ["*", "/", "%"]
  pExp = pBinOp chainr1 pComposeRight $ sstring "^"
  pComposeRight = pBinOp chainl1 pComposeLeft (exactSym "~>")
  pComposeLeft = pBinOp chainr1 pApply (exactSym "<~")

pPrefixSymbol :: Parser Expr
pPrefixSymbol = do
  sym <- pSymbol
  expr <- pRightAssociativeFunction
  return $ Prefix sym expr

pRightAssociativeFunction :: Parser Expr
pRightAssociativeFunction = do
  names <- getRANames >>= (many . choice . map exactStr)
  expr <- optionMaybe pBinary
  case (names, expr) of
    ([], Nothing) -> unexpected "empty input, not an expression"
    (names, Nothing) -> return $ foldr1 Apply (Var <$> names)
    (names, Just expr) -> return $ foldr (Apply . Var) expr names

getRANames :: Parser [Name]
getRANames = getState <!> raNames

addRAssocs :: [Name] -> Parser ()
addRAssocs names = modifyState $ \s -> s {raNames = names ++ raNames s}

pRAssoc :: Parser ()
pRAssoc = do
  keyword "rassoc"
  names <- pIdent (lower <|> upper) `sepBy1` schar ','
  addRAssocs names
  -- this ugly dude is because exactSym, newline and eof return different types
  (exactSym ";" >> return ()) <|> (newline >> return ()) <|> eof

pAnnotation :: Parser ()
pAnnotation = choice [pRAssoc]

pApply :: Parser Expr
pApply = pDeRef >>= parseRest where
  parseRest res = do -- res is a parsed expression
    term <- pDeRef -- run the parser again
    parseRest (Apply res term)
    <|> return res -- at some point the second parse will fail; then
                   -- return what we have so far

pDeRef :: Parser Expr
pDeRef = pDotted >>= parseRest where
  parseRest res = do
    y <- exactSym "[:" *> pExpr <* schar ']'
    parseRest (DeRef res y)
    <|> return res

pDotted :: Parser Expr
pDotted = do
  parseRest =<< (toIgnore >>= \case
    True -> pTerm
    False -> pLambda <|> pTerm)
  where
    parseRest res = do
      exactSym "."
      y <- pTerm
      parseRest (Dot res y)
      <|> return res

pLambdaDot :: Parser Expr
pLambdaDot = LambdaDot <$ exactSym "." <*> pTerm

pCase :: Parser Expr
pCase = Case <$ keyword "case" <*> pExpr <* keyword "of" <*> alts
  where alts = alt `sepBy1` exactSym "|"
        alt = do
          pat <- ignoreOn *> pRightAssociativeFunction <* ignoreOff
          exactSym "=>"
          body <- pExprOrBlock
          return (pat, body)

pTerm :: Parser Expr
pTerm = lexeme $ choice [ Number <$> pDouble
                        , String <$> pString
                        , pLambdaDot
                        , pVar
                        , pConstructor
                        , pParens
                        , pLiteral
                        , pCase ]

pOpenBrace = schar'  '{' >> notFollowedBy (oneOf "!j")
pCloseBrace = schar'  '}'

pBlock :: Parser Expr
pBlock =  choice [ pOpenBrace *> pExpressionsNoWS <* pCloseBrace
                 , (keyword "do" <|> return "") >> pExpression ]

pWhile = While <$ keyword "while" <*> pExpr <*> pBlock
pFor = For <$ keyword "for" <*> pExpr <* keyword "in"
           <*> pExpr <*> pBlock

pExprOrBlock :: Parser Expr
pExprOrBlock = choice [ try pBlock, pIf, pExpr ]

pDefine = choice $ map ($ ("=", Define)) [ pDefBinary, pDefFunction]
pExtend = choice $ map ($ ("&=", Extend)) [ pDefBinary, pDefFunction]

pDefFunction (sym, f) = try $ do
  name <- pIdent lower <|> (schar '(' *> pSymbol <* schar ')')
  args <- many pTerm
  body <- exactSym sym *> pBlock
  return $ f name $ foldr Lambda body args

pDefBinary (sym, f) = try $ do
  arg1 <- pTerm
  op   <- pSymbol
  arg2 <- pTerm
  body <- exactSym sym *> pBlock
  return $ f op $ Lambda (tuple [arg1, arg2]) body

pModified = Modified <$> pMod <*> pExpr

pMod = do
  w <- choice $ map keyword ["mut", "ref", "pure", "local", "lazy"]
  case w of
    "pure"  -> return Pure
    "mut"   -> return Mut
    "ref"   -> return Ref
    "local" -> return Local
    "lazy"  -> return Lazy

pAssign = try $ Assign <$> pExpr <* exactSym ":=" <*> pExprOrBlock

getSame = same
pExpressionsNoWS = do
  exprs <- pExpression `sepEndBy1` (schar ';')
  case exprs of
    [expr] -> return expr
    exprs -> return $ Block exprs

pExpressions = pExpression `sepEndBy1` (getSame <|> schar'  ';')

pIf :: Parser Expr
pIf = do
  keyword "if"
  cond <- pExpr
  true <- pThen
  false <- pElse
  return $ If cond true false

pIfOrIf' :: Parser Expr
pIfOrIf' = do
  keyword "if"
  cond <- pExpr
  true <- pThen
  option (If' cond true) $ do
    false <- pElse
    return $ If cond true false

pLambda = try $ do
  argsBodies <- pArgBody `sepBy1` (exactSym "|")
  case argsBodies of
    [(arg, body)] -> return $ Lambda arg body
    _ -> return $ Lambdas argsBodies
  where pArgBody = (,) <$> pTerm <* exactSym "=>" <*> pExprOrBlock

pBlockOrExpression = try pBlock <|> pExpression
pThen = pBlock <|> do keyword "then" <|> return "then"
                      pExpression
pElse = keyword "else" *> pBlock

pReturn = Return <$ keyword "return" <*> option unit pExpr

pExpression :: Parser Expr
pExpression = do
  many pAnnotation
  expr <- lexeme $ choice $ [ pDefine, pExtend, pAssign
                            , pWhile, pFor, pReturn, pExpr ]
  option expr $ do
    kw <- keyword "after" <|> keyword "before"
    rest <- pBlock
    return $ case kw of
      "after"  -> After expr rest
      "before" -> Before expr rest

pExpr :: Parser Expr
pExpr = lexeme $ choice [ pModified
                        , pLambda
                        , pRightAssociativeFunction
                        , pIfOrIf' ]

testE = parse pExpression
testS = parse pExpressions

pTopLevel = choice [pPrefixSymbol, pExpression, pTypeDef, ObjDec <$> pObjectDec]

grab :: String -> Either ParseError [Expr]
grab = parse pEntryPoint
  where pEntryPoint = pTopLevel `sepEndBy1` (getSame <|> schar'  ';') <* eof

grabT :: String -> Either ErrorList Type
grabT input = case parse pType input of
  Left err -> Left $ ErrorList [T.pack $ show err]
  Right typ -> return typ

grab' input = case grab input of
  Right statements -> map show statements ! intercalate "\n"
  Left err -> error $ show err

grabOrError :: String -> Expr
grabOrError input = case grab input of
  Left err -> error $ show err
  Right exprs -> Block exprs
