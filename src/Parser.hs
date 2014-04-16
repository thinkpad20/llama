{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser ( grab
              , Expr(..)
              , Block
              , grabT
              , grabOrError
              , Parser) where
import Prelude (IO, Eq(..), Ord(..), Bool(..),
                Double, String, Maybe(..), Int, Monad(..),
                ($), (.), floor, map, Functor(..), mapM,
                (+), (-), elem, Either(..), Char, last,
                otherwise, (=<<), Read(..), error, foldl,
                foldr, foldr1)
import qualified Prelude as P
import Text.Parsec hiding (parse, State)
import Control.Applicative hiding (many, (<|>))
import qualified Data.Text as T
import qualified Data.Map as M

import Common
import AST
import TypeLib
import ParserLib

skip :: Parser ()
skip = many (oneOf " \t") *> option () lineComment
  where lineComment = do char '#'
                         many (noneOf "\n")
                         (char '\n' >> return ()) <|> eof

keywords, keySyms :: [String]
keywords = [ "if", "do", "else", "case", "of", "infix", "typedef"
           , "object", "while", "for", "in", "then", "after", "before"
           , "return", "break", "continue", "mut", "ref", "pure"
           , "throw", "try", "catch", "local", "lazy", "with"
           , "block", "forever", "finally"]
keySyms =  ["->", "|", "=", ";", "..", "=>", "?", ":", "#", ":=", "&="]

keyword, exactStr :: Name -> Parser Name
keyword k = fmap T.pack $ go where
  go = lexeme . try $ string (T.unpack k) <* notFollowedBy (alphaNum <|> char '_')

exactStr = keyword

exactSym :: String -> Parser Name
exactSym k = fmap T.pack go where
  go = lexeme . try $ string k <* notFollowedBy (oneOf (symChars <> "."))

lexeme :: Parser a -> Parser a
lexeme p = p <* skip
sstring :: String -> Parser T.Text
sstring s = fmap T.pack (lexeme $ string s)
schar :: Char -> Parser Char
schar c = lexeme $ char c
schar' :: Char -> Parser ()
schar' c = lexeme (char c) >> return ()

check :: Parser String -> Parser String
check p = lexeme . try $ do
  s <- p
  if s `elem` (keywords <> keySyms)
    then unexpected $ "reserved word " <> P.show s
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
  case (first : rest <> bangs) of
    "_" -> unexpected "Single underscore"
    ident | last ident == '-' -> unexpected "Ends in a dash"
          | otherwise -> return ident

pVar :: Parser Expr
pVar = do
  var <- getVar
  option var $ do
    typ <- exactSym ":" *> pType
    return $ Typed var typ
  where getVar = choice [ fmap Var $ try $ pIdent (lower <|> char '_')
                        , wildcard]
        wildcard = WildCard <$ schar '_'

pConstructor :: Parser Expr
pConstructor = Constructor <$> pIdent upper

pDouble :: Parser Double
pDouble = lexeme $ do
  ds <- many1 digit
  option (P.read ds) $ getDecimal ds
  where getDecimal ds = try $ do exactSym "."
                                 ds' <- many1 digit
                                 return $ P.read (ds <> "." <> ds')


pObjectDec :: Parser ObjectDec
pObjectDec = do
  keyword "object"
  name <- pIdent upper
  vars <- many $ fmap T.pack lowers
  extends <- optionMaybe (exactSym "<:" *> pIdent upper)
  (constrs, attrs) <- getConstrs
  return $ defObj {
      objName = name
    , objExtends = extends
    , objVars = vars
    , objConstrs = constrs
    , objAttrs = attrs }

getConstrs :: Parser ([ConstructorDec], [Expr])
getConstrs = (,) <$> constrs <*> attrs where
  constrs = option [] $ exactSym "=" *> pConstructorDec `sepBy` schar '|'
  attrs = option [] $ keyword "with" *> getAttrs
  getAttrs = fmap pure pExpr <|> multi
  multi = between (schar '{') (schar '}') $ pExpr `sepBy1` schar ';'

pConstructorDec :: Parser ConstructorDec
pConstructorDec = do
  name <- pIdent upper
  args <- many pTerm
  extends <- optionMaybe (exactSym "<:" >> pExpr)
  logic <- optionMaybe pExprOrBlock
  return defConstr {
      constrName = name
    , constrArgs = args
    , constrExtends = extends
    , constrLogic = logic }

pType, pTTerm, pTVar, pTConst, pTTuple, pTFunction, pTVector,
  pTApply, pTSet, pTMap, pTList, pTMultiFunc :: Parser Type
pType = choice [pTFunction]
pTTerm = choice [pTMultiFunc, pTVector, pTList, pTMap, pTSet, pTVar, pTConst, pTTuple]
pTVar = TVar <$> fmap T.pack lowers <* skip
pTConst = TConst <$> pIdent upper
pTTuple = pTParens >>= \case
  [t] -> return t
  ts -> return $ tTuple ts
  where pTParens = schar '(' *> sepBy pType (schar ',') <* schar ')'
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
pTMultiFunc = do
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
        exprsAndKwargs = Tuple <$> getExprs <*> kwargs
        kwarg = do
          name <- char '@' *> pIdent lower
          x <- choice [ fmap Left $ exactSym "=" *> pExpr
                      , fmap Right $ exactSym ":" *> pType ]
          return (name, x)
        kwargs = kwarg `sepBy` schar ','

pString :: Parser InString
pString = oneOf "'\"" >>= pString'
pString' :: Char -> Parser InString
pString' start = do
  str <- fmap T.pack (anyChar `manyTill` (lookAhead $ oneOf "\\\"'#"))
  let escape c = (Plain (str <> T.singleton c) <>) <$> pString' start
  oneOf "\\\"'#" >>= \case
    '\\' -> anyChar >>= \case
      'n'  -> escape '\n'
      '\\' -> escape '\\'
      't'  -> escape '\t'
      'r'  -> escape '\r'
      'b'  -> escape '\b'
      '"'  -> escape '"'
      '#'  -> escape '#'
      c | c == start -> escape start
        | c `elem` [' ', '\n', '\t'] -> consume
        | otherwise -> unexpected $ "Unrecognized escape character `" <> [c] <> "'"
      where consume = spaces >> (Plain str <>) <$> (pString' start)
    '#' -> anyChar >>= \case
      '{' -> InterpShow (Plain str) <$> pExprOrBlock <* char '}' <*> pString' start
      '[' -> Interp (Plain str) <$> pExprOrBlock <* char ']' <*> pString' start
    c | c == start -> return (Plain str)
      | c `elem` "'\"" -> escape c
    c -> error $ "wtf is " <> [c]

pLiteral :: Parser Expr
pLiteral = Literal <$> choice [try array, list, try set, dict] where
  commas p = p `sepBy` schar ','
  array = between (schar '[' <* notFollowedBy (char 'l')) (schar ']') go where
    go = try (do
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
pBinary = pUserSym where
  -- | @pBinOp@ takes either @chainl1@ or @chainr1@ for left- or right-
  -- associativity. Takes the higher-precedence parser to try first.
  -- Finally, takes the operator string which if parsed will parse the
  -- expression.
  pBinOp chain higherPrec op = higherPrec `chain` getOp where
    getOp = op >>= \name -> return (\e1 e2 -> binary name e1 e2)
  fold first = foldl (pBinOp chainl1) first . map exactSym
  fold' first = foldl (pBinOp chainr1) first . map exactSym
  pUserSym = pBinOp chainl1 pBackwardApply pSymbol
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
    ([], Nothing) -> unexpected "empty input or invalid expression"
    (_, Nothing) -> return $ foldr1 Apply (Var <$> names)
    (_, Just expr') -> return $ foldr (Apply . Var) expr' names

getRANames :: Parser [Name]
getRANames = getState <!> raNames

addRAssocs :: [Name] -> Parser ()
addRAssocs names = modifyState $ \s -> s {raNames = names <> raNames s}

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
pCase = MultiCase <$ keyword "case" <*> pExpr <* keyword "of" <*> alts
  where alts = alt `sepBy1` exactSym "|"
        alt = do
          pats <- getPat `sepBy1` schar ','
          exactSym "=>"
          body <- pExprOrBlock
          return (pats, body)
        getPat = ignoreOn *> pRightAssociativeFunction <* ignoreOff

pTerm :: Parser Expr
pTerm = do
    term <- go
    option term $ do
      attrs <- many (schar '\\' *> pIdent (upper <|> lower <|> char '_'))
      return (foldl Attribute term attrs)
  where go = lexeme $ choice [ Number <$> pDouble
                             , InString <$> pString
                             , pLambdaDot
                             , pVar
                             , pConstructor
                             , pParens
                             , pLiteral
                             , pCase
                             ]

pOpenBrace, pCloseBrace :: Parser ()
pOpenBrace = schar'  '{' >> notFollowedBy (oneOf "!j")
pCloseBrace = schar'  '}'

pBlock :: Parser Expr
pBlock =  choice [ pOpenBrace *> pExpressionsNoWS <* pCloseBrace
                 , (keyword "do" <|> return "") >> pExpression ]

pFor :: Parser Expr
pFor = try pFor' <|> try pForIn <|> pForever
  where pFor' = For <$ keyword "for" <*> pExpression <* schar ';'
                                     <*> pExpr <* schar ';'
                                     <*> pExpression <*> pBlock
        pForIn = ForIn <$ keyword "for" <*> pExpr <* keyword "in"
                       <*> pExpr <*> pBlock
        pForever = Forever <$ keyword "forever" <*> pBlock

pExprOrBlock :: Parser Expr
pExprOrBlock = choice [ try pBlock, pIf, pExpr ]

pPatternDef :: Parser Expr
pPatternDef = try $ do
  patterns <- pExpr `sepBy1` schar ','
  block <- exactSym "=" *> pBlock
  return $ case patterns of
    [p] -> PatternDef p block
    ps -> PatternDef (tuple ps) block

pDefine, pExtend, pAssign :: Parser Expr
pDefine = pPatternDef
pExtend = choice [ pExtendFunction, pExtendBinary]
pAssign = try $ Assign <$> pExpr <* exactSym ":=" <*> pExprOrBlock

pExtendFunction :: Parser Expr
pExtendFunction = try $ do
  name <- getVar <|> (schar '(' *> pSymbol <* schar ')')
  args <- many pTerm
  body <- exactSym "&=" *> pBlock
  return $ Extend name $ foldr Lambda body args
  where getVar = pIdent (lower <|> char '_')

pExtendBinary :: Parser Expr
pExtendBinary = try $ do
  arg1 <- pTerm
  op   <- pSymbol
  arg2 <- pTerm
  body <- exactSym "&=" *> pBlock
  return $ Extend op $ Lambda (tuple [arg1, arg2]) body

pModified :: Parser Expr
pModified = Modified <$> pMod <*> pExpr
  where pMod = do
          w <- choice $ map keyword ["mut", "ref", "pure", "local", "lazy"]
          case w of
            "pure"  -> return Pure
            "mut"   -> return Mut
            "ref"   -> return Ref
            "local" -> return Local
            "lazy"  -> return Lazy

pExpressionsNoWS :: Parser Expr
pExpressionsNoWS = do
  exprs <- pExpression `sepEndBy1` (schar ';')
  case exprs of
    [expr] -> return expr
    _ -> return $ Block exprs

pIf :: Parser Expr
pIf = do
  keyword "if"
  cond <- pExpr
  trueBranch <- pThen
  option (If' cond trueBranch) $ do
    falseBranch <- pElse
    return $ If cond trueBranch falseBranch

pThen, pElse :: Parser Expr
pThen = pBlock <|> do keyword "then" <|> return "then"
                      pExpression
pElse = keyword "else" *> pBlock

pLambda :: Parser Expr
pLambda = try $ do
  argsBodies <- pArgBody `sepBy1` (exactSym "|")
  case argsBodies of
    [(arg, body)] -> return $ Lambda arg body
    _ -> return $ Lambdas argsBodies
  where pArgBody = (,) <$> pTerm <* exactSym "=>" <*> pExprOrBlock

pReturn :: Parser Expr
pReturn = Return <$ keyword "return" <*> option unit pExpr

pBreak :: Parser Expr
pBreak = Return <$ keyword "break" <*> option unit pExpr

pContinue :: Parser Expr
pContinue = Continue <$ keyword "continue"

pThrow :: Parser Expr
pThrow = Throw <$ keyword "throw" <*> pExpr

pTryCatch :: Parser Expr
pTryCatch = TryCatch <$ keyword "try" <*> pExprOrBlock
                     <*> many catches
                     <*> option Nothing finally
  where finally = keyword "finally" *> (Just <$> pExprOrBlock)
        catches = (,) <$ keyword "catch" <*> pExpr <*> pBlock

pExpression :: Parser Expr
pExpression = do
  many pAnnotation
  expr <- lexeme $ choice $ [ pDefine, pExtend, pAssign
                            , pFor, pReturn, pExpr, pTryCatch
                            , pThrow, pBreak, pContinue ]
  option expr $ do
    kw <- keyword "after" <|> keyword "before"
    rest <- pBlock
    return $ case kw of
      "after"  -> After expr rest
      "before" -> Before expr rest

pExpr :: Parser Expr
pExpr = do
  e <- expr
  withs <- many pWith
  return $ go e (P.reverse withs)
  where go e [] = e
        go e (attrs:rest) = With (go e rest) attrs
        pWith = try $ keyword "with" *> getAttrs
        getAttrs = fmap pure attr <|> between (schar '(') (schar ')') (many1 attr)
        attr = (,) <$> pIdent lower <* exactSym "=" <*> pTerm
        expr = lexeme $ choice [ pModified
                               , pLambda
                               , pRightAssociativeFunction
                               , pIf ]

pTopLevel :: Parser Expr
pTopLevel = choice [pPrefixSymbol, pExpression, pTypeDef, ObjDec <$> pObjectDec]

grab :: String -> Either ParseError Expr
grab input = parse pEntryPoint input >>= \case
    [] -> return unit
    [expr] -> return expr
    exprs  -> return $ Block exprs
  where pEntryPoint = pTopLevel `sepEndBy1` (same <|> schar'  ';') <* eof

grabT :: String -> Either ErrorList Type
grabT input = case parse pType input of
  Left err -> Left $ ErrorList [show err]
  Right typ -> return typ

grabOrError :: String -> Expr
grabOrError input = case grab input of
  Left err -> error $ P.show err
  Right expr -> expr
