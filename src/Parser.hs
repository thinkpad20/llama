{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Prelude as P
import Text.Parsec hiding (satisfy, parse, (<|>), many)
import Data.Set hiding (map)
import qualified Data.Set as S

import Common
import AST
import Tokens
import Tokenizer hiding (item, initState)

data Expr = Expr {_pos :: SourcePos, _expr :: AbsExpr Expr} deriving (P.Show)
instance IsExpr Expr where unExpr (Expr _ e) = e
instance Render Expr where render = render . bareExpr
data ParserState = ParserState {
    _level0ops :: [Name]
  , _level1ops :: [Name]
  , _level2ops :: [Name]
  , _level3ops :: [Name]
  , _level4ops :: [Name]
  , _level5ops :: [Name]
  , _level6ops :: [Name]
  , _level7ops :: [Name]
  , _leftfixities :: Set Name
  , _rightfixities :: Set Name
  }
type Parser = ParsecT [PToken] ParserState Identity

------------------------------------------------------------
-------------------  High-level parsers  -------------------
------------------------------------------------------------

pTopLevel :: Parser Expr
pTopLevel = do
  many same
  pos <- getPosition
  expr <- pExpr
  -- exprs <- pExpr `sepBy` many1 same
  many same
  eof
  -- return $ Expr pos $ Block exprs
  return expr

same = pNodent <|> pPunc ';'
pNodent = satisfy $ \case {Nodent -> True; _ -> False}

pTerm :: Parser Expr
pTerm = choice [pNumber, pVariable, pString, pParens]

pSmallExpr :: Parser Expr
pSmallExpr = pBinaryOp

pExpr :: Parser Expr
pExpr = try pLambda <|> pSmallExpr

pAnyBlock :: Parser Expr
pAnyBlock = pBlock <|> pInlineBlock

pInlineBlock = item $ Block <$> pExpr `sepBy1` pPunc ';'

pBlock = item $ indented $ Block <$> blockOf pExpr

blockOf p = p `sepBy1` same

indented p = indent *> p <* outdent where
  indent = satisfy $ \case {Indent -> True; _ -> False}
  outdent = satisfy $ \case {Outdent -> True; _ -> False}

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

pPunc :: Char -> Parser Token
pPunc c = satisfy $ \case {TPunc c' | c == c' -> True; _ -> False}

pAnyIdent :: Parser Name
pAnyIdent = satisfy go >>= return . unbox where
  go = \case {TId _ -> True; TKeyword _ -> True; _ -> False}
  unbox = \case {TId n -> n; TKeyword n -> n}

pExactSym :: Name -> Parser Name
pExactSym name = satisfy go >>= return . unbox where
  go = \case {TSymbol n | n == name -> True; _ -> False}
  unbox = \case TSymbol n -> n

pNumber, pVariable, pString :: Parser Expr
pNumber = item $ num >>= \case
  TInt i -> return $ Number $ fromIntegral i
  TFloat n -> return $ Number n
  where num = satisfy (\case {TInt _ -> True; TFloat _ -> True; _ -> False})
pVariable = item $ var >>= \(TId n) -> return $ Var n
  where var = satisfy (\case {TId _ -> True; _ -> False})
pString = item $ str >>= \(TStr n) -> return $ String n
  where str = satisfy (\case {TStr _ -> True; _ -> False})

pChain :: Parser Expr
pChain = pTerm >>= go where
  go expr = option expr $ do
    lookAhead next >>= \pt -> case tToken pt of
      TPunc '(' | not (tHasSpace pt)-> do
        -- Grab the arguments, then recurse
        tup <- pParens
        go $ Expr (_pos expr) $ Apply expr tup
      -- If there is a square bracket, it's an object dereference.
      TPunc '[' | not (tHasSpace pt) -> do
        -- Grab the arguments, then recurse.
        ref <- pPunc '[' *> pExpr <* pPunc ']'
        go $ Expr (_pos expr) $ DeRef expr ref
      TPunc '.' -> do
        dotfunc <- pPunc '.' *> pExpr
        go $ Expr (_pos expr) $ Dot expr dotfunc
      _ -> return expr

pApply :: Parser Expr
pApply = pChain >>= parseRest where
  parseRest res = do -- res is a parsed expression
    term <- pChain   -- run the parser again
    parseRest (Expr (_pos res) (Apply res term))
    <|> return res   -- at some point the second parse will fail; then
                     -- return what we have so far

pParens :: Parser Expr
pParens = item $ pPunc '(' >> do
  ex <- pExpr
  choice [getBlock ex, getTuple ex, return $ unExpr ex] <* pPunc ')'
  where
    getBlock ex = (pPunc ';' <|> same) *> do
      exprs <- pExpr `sepBy` same
      return $ Block (ex:exprs)
    getTuple ex = pPunc ',' *> do
      exprs <- pExpr `sepBy` pPunc ','
      return $ Tuple (ex:exprs) mempty

---------------------------------------------------
---------------  Binary operators  ----------------
---------------------------------------------------

-- | Entry point for binary operators.
pBinaryOp :: Parser Expr
pBinaryOp = pLevel0

pLevel0, pLevel1, pLevel2, pLevel3, pLevel4,
  pLevel5, pLevel6, pLevel7 :: Parser Expr
pLevel0 = pLevel _level0ops pLevel1
pLevel1 = pLevel _level1ops pLevel2
pLevel2 = pLevel _level2ops pLevel3
pLevel3 = pLevel _level3ops pLevel4
pLevel4 = pLevel _level4ops pLevel5
pLevel5 = pLevel _level5ops pLevel6
pLevel6 = pLevel _level6ops pLevel7
pLevel7 = pLevel _level7ops pApply

pLevel:: (ParserState -> [Text]) -> Parser Expr -> Parser Expr
pLevel ops higher = do
  ops <- getState <!> ops
  pLeftBinary ops higher

-- | Right-associative binary parser. Takes a list of operators, and the
-- next-higher-precedence parser to run first.
pRightBinary :: [Text] -> Parser Expr -> Parser Expr
pRightBinary ops higher = higher >>= go where
  go left = option left $ choice (map pExactSym ops) >>= \op -> do
    right <- pLeftBinary ops higher
    go $ Expr (_pos left) $ Binary op left right

-- | Left-associative binary parser. Takes a list of operators, and the
-- next-higher-precedence parser to run first.
pLeftBinary :: [Text] -> Parser Expr -> Parser Expr
pLeftBinary ops higher = chainl1 higher go where
  go = choice (map pExactSym ops) >>= go'
  go' op = pure $ \e1 -> Expr (_pos e1) . Binary op e1

---------------------------------------------------------
---------------------  Definitions  ---------------------
---------------------------------------------------------

pLambda :: Parser Expr
pLambda = item $ do
  param <- pSmallExpr
  pExactSym "=>"
  body <- pAnyBlock
  return $ Lambda param body

---------------------------------------------------------
-----------------------  Helpers  -----------------------
---------------------------------------------------------

grab :: (PToken -> Maybe a) -> Parser a
grab = token show tPosition

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = satisfy' (f . tToken)

satisfy' :: (PToken -> Bool) -> Parser Token
satisfy' f = grab (\pt -> if f pt then Just (tToken pt) else Nothing)

next :: Parser PToken
next = grab Just

tuple :: SourcePos -> [Expr] -> Expr
tuple pos exprs = Expr pos $ Tuple exprs mempty

item :: Parser (AbsExpr Expr) -> Parser Expr
item p = Expr <$> getPosition <*> p

----------------------------------------------------------
-----------------  Running the parser  -------------------
----------------------------------------------------------

initState = ParserState {
  _level0ops       = ["$", "!"]
  , _level1ops     = []
  , _level2ops     = ["&&", "||", "|^|"]
  , _level3ops     = [">", "<", "<=", ">=", "==", "!="]
  , _level4ops     = ["+", "-"]
  , _level5ops     = ["*", "/"]
  , _level6ops     = ["**", "^"]
  , _level7ops     = ["~>", "<~"]
  , _leftfixities  = S.fromList ["!", "+", "-", "*", "/"]
  , _rightfixities = S.fromList ["**", "^", "&&", "||", "|^|"]
  }

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser input = case tokenize input of
  Left err -> Left err
  Right tokens -> runIdentity $ runParserT parser initState "" tokens

parse :: String -> Either ParseError Expr
parse = parseWith pTopLevel

-- see :: String -> IO ()
seeWith parser input = case parseWith parser input of
  Left err -> error $ show err
  Right ast -> P.putStrLn $ unpack $ render ast

see = seeWith pTopLevel
