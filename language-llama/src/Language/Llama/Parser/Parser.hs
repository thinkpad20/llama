{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Llama.Parser.Parser (parse, Expr, Sourced(..)) where

import qualified Prelude as P
import Text.Parsec hiding (satisfy, parse, (<|>), many)
import Data.Set hiding (map)
import qualified Data.Set as S
import qualified Data.Text as T

import Language.Llama.Common.Common
import Language.Llama.Common.AST
import Language.Llama.Parser.Tokens
import Language.Llama.Parser.Tokenizer hiding (item, initState)

data Expr = Expr {_pos :: SourcePos, _expr :: AbsExpr Expr}
            deriving (P.Show, Eq)
instance IsExpr Expr where unExpr (Expr _ e) = e
instance Render Expr where render = rndr True . bareExpr
class IsExpr e => Sourced e where source :: e -> SourcePos
instance Sourced Expr where source = _pos
data ParserState = ParserState
  { _level0ops     :: [Name]
  , _level1ops     :: [Name]
  , _level2ops     :: [Name]
  , _level3ops     :: [Name]
  , _level4ops     :: [Name]
  , _level5ops     :: [Name]
  , _level6ops     :: [Name]
  , _level7ops     :: [Name]
  , _level8ops     :: [Name]
  , _knownSymbols  :: Set Name
  , _leftfixities  :: Set Name
  , _rightfixities :: Set Name }
type Parser = ParsecT [PToken] ParserState Identity

------------------------------------------------------------
-------------------  High-level parsers  -------------------
------------------------------------------------------------

pTopLevel :: Parser Expr
pTopLevel = unblock <$> pBlockOf pStatement <* many same <* eof

-- | The simplest single parsed unit.
pTerm :: Parser Expr
pTerm = choice [ pNumber, pVariable, pConstructor, pString, pParens
               , pArray, pDictOrSet, pJson]

-- | A "small expr" is simpler than a lambda or similar.
pSmallExpr :: Parser Expr
pSmallExpr = pBinaryOp

-- | Any expr, which could be a lambda, or definition, etc.
pExpr :: Parser Expr
pExpr = pIf <|> pUnless <|> pFor <|> pCase <|> pMod <|> pLambdaDot where
  pMod = item mod <|> pExpr'
  mod = do
    m <- choice $ map pKeyword ["ref", "array", "hash"]
    Modified m <$> pMod
  pExpr' = do
    expr <- pSmallExpr
    option expr $ fmap (\f -> f expr) extra
  extra = choice [ pLambda, pPatternDef, pPostIf, pPostUnless, pPostFor
                 , pAfter, pBefore]

-- | More high-level than expressions. Things like object/trait declarations.
pStatement = choice [pObjectDec, pTraitDec, pExpr]

---------------------------------------------------------
--------------------  Control flow  ---------------------
---------------------------------------------------------

pIf :: Parser Expr
pIf = item $ If <$> cond <*> true <*> false where
  cond = pKeyword "if" *> pSmallExpr
  true = pKeyword "then" *> pAnyBlock <|> pIndentedBlock
  false = optionMaybe $ pKeyword "else" *> pAnyBlock

pUnless :: Parser Expr
pUnless = item $ Unless <$> cond <*> true <*> false where
  cond = pKeyword "unless" *> pSmallExpr
  true = pKeyword "then" *> pAnyBlock <|> pIndentedBlock
  false = optionMaybe $ pKeyword "else" *> pAnyBlock

pFor :: Parser Expr
pFor = item $ do
  for <- pForPattern
  body <- pKeyword "do" *> pAnyBlock <|> pIndentedBlock
  return $ For for body

pPostIf :: Parser (Expr -> Expr)
pPostIf = do
  cond <- pKeyword "if" *> pExpr
  els <- optionMaybe $ pKeyword "else" *> pExpr
  return $ \expr -> Expr (_pos expr) $ PostIf expr cond els

pPostUnless :: Parser (Expr -> Expr)
pPostUnless = do
  cond <- pKeyword "unless" *> pExpr
  els <- optionMaybe $ pKeyword "else" *> pExpr
  return $ \expr -> Expr (_pos expr) $ PostUnless expr cond els

pPostFor :: Parser (Expr -> Expr)
pPostFor = do
  for <- pForPattern
  return $ \expr -> Expr (_pos expr) $ ForComp expr for

pAfter :: Parser (Expr -> Expr)
pAfter = do
  rest <- pKeyword "after" *> pExprOrBlock
  return $ \expr -> Expr (_pos expr) $ After expr rest

pBefore :: Parser (Expr -> Expr)
pBefore = do
  rest <- pKeyword "before" *> pExprOrBlock
  return $ \expr -> Expr (_pos expr) $ After expr rest

pCase :: Parser Expr
pCase = item $ MultiCase <$> expr <*> getAlts where
  expr = pKeyword "case" *> pExpr <* pKeyword "of"
  getAlts = pAnyBlockOf alt
  alt = do
    exprs <- pSmallExpr `sepBy1` pSymbol "|"
    result <- pSymbol "->" *> pExprOrBlock
    return (exprs, result)

pForPattern :: Parser (ForPattern Expr)
pForPattern = forever <|> for where
  forever = pKeyword "forever" >> return Forever
  for = do
    expr <- pKeyword "for" *> pSmallExpr
    let for = do test <- pPunc ';' *> pSmallExpr
                 step <- pPunc ';' *> pSmallExpr
                 return $ For_ expr test step
        forIn = do cont <- pKeyword "in" *> pSmallExpr
                   return $ ForIn expr cont
    option (ForExpr expr) $ for <|> forIn

pLambdaDot :: Parser Expr
pLambdaDot = item $ pPunc '.' >> LambdaDot <$> pChain <*> many pChain

---------------------------------------------------------
---------------------  Definitions  ---------------------
---------------------------------------------------------

pLambda :: Parser (Expr -> Expr)
pLambda = do
  pSymbol "->"
  body <- pAnyBlock
  let single param = Expr (_pos param) $ Lambdas [(param, body)]
  option single $ lookAhead next >>= \t -> case tToken t of
    TSymbol "|" -> do
      pSymbol "|"
      bodies <- alt `sepBy1` pSymbol "|"
      return $ \param -> Expr (_pos param) $ Lambdas ((param, body):bodies)
    Indent -> do
      bodies <- indented $ pBlockOf alt
      return $ \param -> Expr (_pos param) $ Lambdas ((param, body):bodies)
    _ -> return single
  where alt = do
          expr <- pSmallExpr
          result <- pSymbol "->" *> pExprOrBlock
          return (expr, result)

pPatternDef :: Parser (Expr -> Expr)
pPatternDef = do
  pSymbol "="
  expr <- pExprOrBlock
  return $ \pat -> Expr (_pos pat) $ PatternDef pat expr

pObjectDec :: Parser Expr
pObjectDec = item $ ObjDec <$> do
  pKeyword "type"
  name <- pConstrName
  vars <- commaSep pVarName
  extends <- optionMaybe $ pSymbol "<:" *> pConstrName
  (constrs, attrs) <- getConstrs
  return $ defObj { objName = name
                  , objExtends = extends
                  , objVars = vars
                  , objConstrs = constrs
                  , objAttrs = attrs }

getConstrs :: Parser ([ConstructorDec Expr], [Expr])
getConstrs = (,) <$> constrs <*> attrs where
  constrs = option [] $ pSymbol "=" *> pAnyBlockOf pConstructorDec
  attrs = option [] $ pKeyword "with" *> pAnyBlockOf pExpr

pConstructorDec :: Parser (ConstructorDec Expr)
pConstructorDec = do
  name <- pConstrName
  args <- many pTerm
  extends <- optionMaybe (pSymbol "<:" >> pSmallExpr)
  return defConstr { constrName = name
                   , constrArgs = args
                   , constrExtends = extends }

-- | Defines a trait.
pTraitDec :: Parser Expr
pTraitDec = unexpected "Trait declarations not yet implemented"

------------------------------------------------------------
-------------------------  Types  --------------------------
------------------------------------------------------------

-- pType :: Parser Type
-- pType = pTVar
--
-- pTVar :: Parser Type
-- pTVar =

------------------------------------------------------------
-------------------------  Blocks  -------------------------
------------------------------------------------------------

-- | AnyBlock means either an indented block or an inline block.
pAnyBlock :: Parser Expr
pAnyBlock = pIndentedBlock <|> pInlineBlock

-- | An expression, or a block.
pExprOrBlock :: Parser Expr
pExprOrBlock = pExpr <|> pKeyword "do" *> pAnyBlock

-- | @pAnyBlock@ but generalized to any parser.
pAnyBlockOf :: Parser a -> Parser [a]
pAnyBlockOf p = semiSep1 p <|> indented (pBlockOf p)

-- | A block that doesn't start with an indent. Separated with @;@.
pInlineBlock :: Parser Expr
pInlineBlock = unblock <$> semiSep1 pExpr

-- | An indented block, separators are same indentation OR semicolons.
pIndentedBlock :: Parser Expr
pIndentedBlock = indented $ unblock <$> pBlockOf pExpr

-- | Grabs one or more @p@s separated by semicolon or same indentation.
pBlockOf :: Parser a -> Parser [a]
pBlockOf p = p `sepEndBy1` many1 same

------------------------------------------------------------
------------------------  Literals  ------------------------
------------------------------------------------------------

pArray :: Parser Expr
pArray = item $ Literal <$> enclose '[' ']' go where
  go = option (VecLiteral []) $ do
    first <- pExpr
    option (VecLiteral [first]) $ do
      pSymbol ".."
      last <- pExpr
      return $ VecRange first last
      <|> do pPunc ','
             rest <- pExpr `sepEndBy` pPunc ','
             return $ VecLiteral $ first : rest

-- | Grabs either a dict or a set. If it's empty, by default it's a set.
pDictOrSet :: Parser Expr
pDictOrSet = item $ Literal <$> enclose '{' '}' go where
  go = option (DictLiteral []) $ do
    first <- pSmallExpr
    option (SetLiteral [first]) $ do
      pSymbol "=>" *> dict first
      <|> pPunc ',' *> set first
  dict first = do
    val <- (,) first <$> pExpr
    option (DictLiteral [val]) $ do
      pPunc ','
      rest <- keyval `sepEndBy` pPunc ','
      return $ DictLiteral $ val : rest
  set first = do
    rest <- pExpr `sepEndBy` pPunc ','
    return $ SetLiteral $ first : rest
  keyval = (,) <$> pSmallExpr <* pSymbol "=>" <*> pExpr

-- | Grabs an embedded JSON literal object.
pJson :: Parser Expr
pJson = item $ Literal . JsonLiteral <$> do
  pKeyword "json" *> jObject where
  jVal = jArray <|> jObject <|> jString <|> jNum <|> jKeyword
  keyval = (,) <$> fmap unString pTerm <*> (pSymbol ":" *> jVal)
  jArray = JArray <$> enclose '[' ']' (commaSep jVal)
  jObject = JObject <$> enclose '{' '}' (commaSep keyval)
  jString = JString . unString <$> pString
  jNum = fmap JNum $ fmap unExpr pNumber >>= \case Number n -> return n
  jKeyword = try $ fmap unExpr pVariable >>= \case
    Var "true" -> return $ JBool True
    Var "false" -> return $ JBool False
    Var "null" -> return $ JNull
    e -> unexpected $ (unpack $ render e) <> " is not a JSON keyword"

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

-- | A unit of punctuation, like ( or ;
pPunc :: Char -> Parser (Token PToken)
pPunc c = satisfy $ \case {TPunc c' | c == c' -> True; _ -> False}

-- | A keyword.
pKeyword :: Name -> Parser Name
pKeyword name = satisfy go >>= \case TKeyword k -> return k
  where go = \case {TKeyword n | n == name -> True; _ -> False}

-- | Any identifier (including keywords).
pAnyIdent :: Parser Name
pAnyIdent = satisfy go >>= return . unbox where
  go = \case {TId _ -> True; TKeyword _ -> True; _ -> False}
  unbox = \case {TId n -> n; TKeyword n -> n}

pVarName, pConstrName, pTVarName :: Parser Name
pVarName = var >>= \(TId n) -> return n where
  var = satisfy $ \case {TId n | n!T.head!isUpper!not -> True; _ -> False}
pConstrName = var >>= \(TId n) -> return n where
  var = satisfy $ \case {TId n | n!T.head!isUpper -> True; _ -> False}
pTVarName = var >>= \(TId n) -> return n where
  test n = (n!T.head!isUpper!not) && (n!T.any isDigit!not)
  var = satisfy $ \case {TId n | test n -> True; _ -> False}

-- | Parses the exact symbol requested.
pSymbol :: Name -> Parser Name
pSymbol name = satisfy go >>= \case TSymbol n -> return n
  where go = \case {TSymbol n | n == name -> True; _ -> False}

-- | Parses any symbol.
pAnySym :: Parser Name
pAnySym = satisfy go >>= \case TSymbol n -> return n
  where go = \case {TSymbol _ -> True; _ -> False}

-- | Numbers, variables, strings. Munches interpolated strings as well.
pNumber, pVariable, pString :: Parser Expr
pNumber = item $ num >>= \case
  TInt i -> return $ Int i
  TFloat n -> return $ Number n
  where num = satisfy (\case {TInt _ -> True; TFloat _ -> True; _ -> False})
pVariable = item $ Var <$> pVarName
pConstructor = item $ var >>= \(TId n) -> return $ Constructor n
  where var = satisfy test
        test = \case {TId n | n!T.head!isUpper -> True; _ -> False}
pString = item $ str >>= go where
  go :: Token PToken -> Parser (AbsExpr Expr)
  go tkn = case tkn of
    TStr s -> return $ String s
    TIStr istr -> InString <$> getIstr istr
  getIstr :: IStr PToken -> Parser (InString Expr)
  getIstr = \case
    Plain s -> return $ Bare s
    IStr is1 tkns is2 -> do
      is1' <- getIstr is1
      is2' <- getIstr is2
      pos <- getPosition
      case parseFrom pos tkns of
        Left err -> unexpected $ "Error in interpolated string: " <> P.show err
        Right (expr, pos') -> do
          setPosition pos'
          return $ InterpShow is1' expr is2'
  str = satisfy (\case {TStr _ -> True; TIStr _ -> True; _ -> False})

-- | Using the backslash to dereference an attribute off of an object.
pAttribute :: Parser Expr
pAttribute = pTerm >>= go where
  go term = option term $ do
    pPunc '\\'
    name <- pAnyIdent
    let term' = Attribute term name
        epos = Expr (_pos term)
    go $ epos term'

-- | Parses a term, possibly followed by a few things, such as: a dot,
-- brackets without a space (object deref), parens without a space (high-
-- precedence application), assignment operators (:=, +=, etc), or a @with@.
pChain :: Parser Expr
pChain = pAttribute >>= go where
  go expr = option expr $ do
    let go' = go . mk
        mk = Expr (_pos expr)
        --objDeref = immediate $ go' =<< DeRef expr <$> enclose '[' ']' pExpr
        --highApp = immediate $ go' =<< Apply expr <$> pParens
    lookAhead next >>= \pt -> case tToken pt of
      -- If there is an immediate square bracket, it's an object dereference.
      TPunc '[' | not (tHasSpace pt) -> do
        -- Grab the arguments, then recurse.
        ref <- enclose '[' ']' pExpr
        go' $ DeRef expr ref
      -- If there's an immediate parentheses, it's high-precedence application.
      TPunc '(' | not (tHasSpace pt) -> do
        arg <- pParens
        go' $ Apply expr arg
      TPunc '.' -> do
        dotfunc <- pPunc '.' *> pTerm
        go' $ Dot expr dotfunc
      -- If there's an immediate symbol, it's either a postfix expression, or
      -- it's a high-precedence binary operation (which would be another
      -- immediate expression).
      TSymbol _ | not (tHasSpace pt) -> do
        op <- pAnySym
        option (mk $ Postfix expr op) $ do
          go' =<< Binary op expr <$> immediate pAttribute
      TSymbol ".=" -> do
        -- Symbol .= is a special case because it's syntactic sugar, not just
        -- a function.
        pSymbol ".="
        fmap (Expr $ _pos expr) $ AssignDot expr <$> pChain <*> many pChain
      TSymbol _ -> do
        -- The level 8 operators are assignment operators (:=, +=, *=, etc).
        ops <- _level8ops <$> getState
        -- Note that if this choice fails we're OK, since we used lookAhead.
        op <- choice $ map pSymbol ops
        Expr (_pos expr) . Binary op expr <$> pExpr
      TKeyword "with" -> do
        let attr = do name <- pAnyIdent
                      val <- pSymbol "=" *> pPrefixOp
                      return (name, val)
        attrs <- pKeyword "with" *> commaSep1 attr
        go' $ With expr attrs
      _ -> return expr

-- | Parses function application. Lower precedence than dot or object ref.
pApply :: Parser Expr
pApply = pChain >>= parseRest where
  parseRest res = do -- res is a parsed expression
    term <- pChain   -- run the parser again
    parseRest (Expr (_pos res) (Apply res term))
    <|> return res   -- at some point the second parse will fail; then
                     -- return what we have so far

-- | Parses unary prefix operators.
pPrefixOp :: Parser Expr
pPrefixOp = item unary <|> pApply where
  unary = try $ Prefix <$> pAnySym <*> immediate pApply

-- | Parses something in parentheses; a single expression or tuple.
pParens :: Parser Expr
pParens = item $ enclose '(' ')' $ do
  lookAhead next >>= \t -> case tToken t of
    TKeyword "do" -> pKeyword "do" >> unExpr <$> pInlineBlock
    _ -> do
      exprs <- pExpr `sepEndBy` pPunc ','
      kwargs <- option [] $ pPunc ';' *> commaSep kwarg
      case (exprs, kwargs) of
        ([e], []) -> return $ unExpr e
        (es, ks) -> return $ Tuple exprs kwargs
      where
        kwarg = Kwarg <$> var <*> typ <*> expr
        var = pVarName
        typ = pure Nothing
        expr = pSymbol "=" *> optionMaybe pExpr

---------------------------------------------------
---------------  Binary operators  ----------------
---------------------------------------------------

-- | Entry point for binary operators.
pBinaryOp :: Parser Expr
pBinaryOp = pUnknownBinary

pLevel0, pLevel1, pLevel2, pLevel3, pLevel4,
  pLevel5, pLevel6, pLevel7 :: Parser Expr
-- | Piping operators, like <| and |>
pLevel0 = pLevel _level0ops pLevel1
-- | No operators by default in level 1.
pLevel1 = pLevel _level1ops pLevel2
-- | Logical operators.
pLevel2 = pLevel _level2ops pLevel3
-- | Comparison operators.
pLevel3 = pLevel _level3ops pLevel4
-- | Additive operators.
pLevel4 = pLevel _level4ops pLevel5
-- | Multiplicative operators.
pLevel5 = pLevel _level5ops pLevel6
-- | Exponentiation operators.
pLevel6 = pLevel _level6ops pLevel7
-- | Function composition operators (~>, <~).
pLevel7 = pLevel _level7ops pPrefixOp

pLevel:: (ParserState -> [Text]) -> Parser Expr -> Parser Expr
pLevel ops higher = do
  ops <- getState <!> ops
  pLeftBinary ops higher

-- | Grabs any binary, i.e. one that won't get caught by a later parser.
pUnknownBinary :: Parser Expr
pUnknownBinary = pLevel0 >>= go where
  go left = option left $ try $ pAnySym >>= \op -> do
    known <- _knownSymbols <$> getState
    when (op `S.member` known) $ do
      unexpected $ unpack $ "Symbol " <> op <> " with known precedence"
    right <- pUnknownBinary
    go $ Expr (_pos left) $ Binary op left right

-- | Right-associative binary parser. Takes a list of operators, and the
-- next-higher-precedence parser to run first.
pRightBinary :: [Text] -> Parser Expr -> Parser Expr
pRightBinary ops higher = higher >>= go where
  go left = option left $ choice (map pSymbol ops) >>= \op -> do
    right <- pLeftBinary ops higher
    go $ Expr (_pos left) $ Binary op left right

-- | Left-associative binary parser. Takes a list of operators, and the
-- next-higher-precedence parser to run first.
pLeftBinary :: [Text] -> Parser Expr -> Parser Expr
pLeftBinary ops higher = chainl1 higher go where
  go = choice (map pSymbol ops) >>= go'
  go' op = pure $ \e1 -> Expr (_pos e1) . Binary op e1

---------------------------------------------------------
-----------------------  Helpers  -----------------------
---------------------------------------------------------

grab :: (PToken -> Maybe a) -> Parser a
grab = token (render ~> unpack) tPosition

satisfy :: (Token PToken -> Bool) -> Parser (Token PToken)
satisfy f = satisfy' (f . tToken)

satisfy' :: (PToken -> Bool) -> Parser (Token PToken)
satisfy' f = grab (\pt -> if f pt then Just (tToken pt) else Nothing)

-- | Satisfies if the parser succeeds AND the first token had no spaces.
immediate :: Parser a -> Parser a
immediate p = lookAhead next >>= \pt -> do
  when (tHasSpace pt) (unexpected "Leading space") >> p

-- | Grabs the next token regardless of what it is (besides EOF)
next :: Parser PToken
next = grab Just

tuple :: SourcePos -> [Expr] -> Expr
tuple pos exprs = Expr pos $ Tuple exprs mempty

item :: Parser (AbsExpr Expr) -> Parser Expr
item p = Expr <$> getPosition <*> p

-- | Separators between statements or other discrete units.
same :: Parser (Token PToken)
same = pNodent <|> pPunc ';'

pNodent :: Parser (Token PToken)
pNodent = satisfy $ \case {Nodent -> True; _ -> False}

-- | Grabs @p@ between an indent/outdent pair.
indented :: Parser a -> Parser a
indented p = indent *> p <* outdent where
  indent = satisfy $ \case {Indent -> True; _ -> False}
  outdent = satisfy $ \case {Outdent -> True; _ -> False}

enclose :: Char -> Char -> Parser a -> Parser a
enclose c1 c2 = between (pPunc c1) (pPunc c2)

unString :: Expr -> Text
unString e = case unExpr e of
  String s -> s
  Int n -> render n
  Number n -> render n
  Var n -> n
  Constructor n -> n
  _ -> error "Not a string"

unblock :: [Expr] -> Expr
unblock [] = error "Empty block"
unblock [e] = e
unblock (e:es) = Expr (_pos e) $ e `Then` unblock es

commaSep, commaSep1, semiSep1 :: Parser a -> Parser [a]
commaSep p = p `sepBy` pPunc ','
commaSep1 p = p `sepBy1` pPunc ','
semiSep1 p = p `sepBy1` pPunc ';'

----------------------------------------------------------
-----------------  Running the parser  -------------------
----------------------------------------------------------

initState = ParserState {
  _level0ops       = ["<|", "|>"]
  , _level1ops     = []
  , _level2ops     = ["&&", "||", "|^|"]
  , _level3ops     = [">", "<", "<=", ">=", "==", "!="]
  , _level4ops     = ["+", "-"]
  , _level5ops     = ["*", "/"]
  , _level6ops     = ["**", "^"]
  , _level7ops     = ["~>", "<~"]
  , _level8ops = [":=", "+=", "*=", "/=", "-=", "<>=", "^="]
  , _knownSymbols  = S.fromList [ "$", "!", "&&", "||", "|^|", ">", ">", "<="
                                , ">=", "==", "!=", "+", "-", "*", "/", "**"
                                , "^", "~>", "~>", ":=", "->", "=>", "..", "="
                                , "~", "+=", "*=", "/=", "-=", "<>=", "^="
                                , "|"]
  , _leftfixities  = S.fromList ["!", "+", "-", "*", "/"]
  , _rightfixities = S.fromList ["**", "^", "&&", "||", "|^|"]
  }

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser input = case tokenize input of
  Left err -> Left err
  Right tokens -> runIdentity $ runParserT parser initState "" tokens

parse :: String -> Either ParseError Expr
parse = parseWith pTopLevel

testStringWith :: Render a => Parser a -> String -> IO ()
testStringWith parser input = case parseWith parser input of
  Left err -> error $ P.show err
  Right ast -> P.putStrLn $ unpack $ render ast

testString :: String -> IO ()
testString = testStringWith pTopLevel

-- | Tokenizes from a given starting position. Returns the ending position.
parseFrom :: SourcePos
          -> [PToken]
          -> Either ParseError (Expr, SourcePos)
parseFrom pos = runParserFrom pos pExpr

-- | Runs a tokenizer given a starting source position. Is used when we
-- parse a list of tokens inside of a string interpolation.
runParserFrom :: SourcePos   -- ^ Position from which to start
              -> Parser a    -- ^ The tokenizer to run
              -> [PToken]    -- ^ The input string
              -> Either ParseError (a, SourcePos) -- ^ Result and new pos
runParserFrom pos parser input = do
  let parser' = do
        setPosition pos
        res <- parser
        pos' <- getPosition
        return (res, pos')
  runIdentity $ runParserT parser' initState "" input
