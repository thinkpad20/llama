{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Llama.Parser.Parser where

import qualified Prelude as P
import Text.Parsec hiding (satisfy, parse, (<|>), many)
import Data.Set hiding (map)
import qualified Data.Set as S
import Debug.Trace

import Language.Llama.Common.Common
import Language.Llama.Common.AST
import Language.Llama.Parser.Tokens
import Language.Llama.Parser.Tokenizer hiding (item, initState)

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
  pos <- many same *> getPosition
  exprs <- option [] $ blockOf pExpr
  many same *> eof
  -- return $ Expr pos $ Block exprs
  return $ Expr pos $ Block exprs

-- | The simplest single parsed unit.
pTerm :: Parser Expr
pTerm = choice [pNumber, pVariable, pString, pParens]

-- | A "small expr" is simpler than a lambda or similar.
pSmallExpr :: Parser Expr
pSmallExpr = pBinaryOp

-- | Any expr, which could be a lambda, or definition, etc.
pExpr :: Parser Expr
pExpr = do
  expr <- pSmallExpr
  option expr $ choice [pLambda expr, pPatternDef expr]
  --try pLambda <|> pSmallExpr

------------------------------------------------------------
-------------------------  Blocks  -------------------------
------------------------------------------------------------

-- | AnyBlock means either an indented block or an inline block.
pAnyBlock :: Parser Expr
pAnyBlock = pIndentedBlock <|> pInlineBlock

-- | A block that doesn't start with an indent. Separated with @;@.
pInlineBlock :: Parser Expr
pInlineBlock = item $ Block <$> pExpr `sepBy1` pPunc ';'

-- | An intented block, separators are same indentation OR semicolons.
pIndentedBlock :: Parser Expr
pIndentedBlock = item $ indented $ Block <$> blockOf pExpr

-- | Grabs one or more @p@s separated by semicolon or same indentation.
blockOf :: Parser a -> Parser [a]
blockOf p = p `sepBy1` same

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

-- | A unit of punctuation, like ( or ;
pPunc :: Char -> Parser (Token PToken)
pPunc c = satisfy $ \case {TPunc c' | c == c' -> True; _ -> False}

-- | Any identifier (including keywords).
pAnyIdent :: Parser Name
pAnyIdent = satisfy go >>= return . unbox where
  go = \case {TId _ -> True; TKeyword _ -> True; _ -> False}
  unbox = \case {TId n -> n; TKeyword n -> n}

-- | Parses the exact symbol requested.
pExactSym :: Name -> Parser Name
pExactSym name = satisfy go >>= return . unbox where
  go = \case {TSymbol n | n == name -> True; _ -> False}
  unbox = \case TSymbol n -> n

-- | Numbers, variables, strings. Munches interpolated strings as well.
pNumber, pVariable, pString :: Parser Expr
pNumber = item $ num >>= \case
  TInt i -> return $ Number $ fromIntegral i
  TFloat n -> return $ Number n
  where num = satisfy (\case {TInt _ -> True; TFloat _ -> True; _ -> False})
pVariable = item $ var >>= \(TId n) -> return $ Var n
  where var = satisfy (\case {TId _ -> True; _ -> False})
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
        Left err -> unexpected $ "Error in interpolated string: " <> show err
        Right (expr, pos') -> do
          setPosition pos'
          return $ InterpShow is1' expr is2'
  str = satisfy (\case {TStr _ -> True; TIStr _ -> True; _ -> False})

-- | Parses a term, possibly followed by a dot or brackets.
pChain :: Parser Expr
pChain = pTerm >>= go where
  go expr = option expr $ do
    lookAhead next >>= \pt -> case tToken pt of
      -- If there is an immediate square bracket, it's an object dereference.
      TPunc '[' | not (tHasSpace pt) -> do
        -- Grab the arguments, then recurse.
        ref <- pPunc '[' *> pExpr <* pPunc ']'
        go $ Expr (_pos expr) $ DeRef expr ref
      TPunc '.' -> do
        dotfunc <- pPunc '.' *> pExpr
        go $ Expr (_pos expr) $ Dot expr dotfunc
      _ -> return expr

-- | Parses function application. Lower precedence than dot or object ref.
pApply :: Parser Expr
pApply = pChain >>= parseRest where
  parseRest res = do -- res is a parsed expression
    term <- pChain   -- run the parser again
    parseRest (Expr (_pos res) (Apply res term))
    <|> return res   -- at some point the second parse will fail; then
                     -- return what we have so far

-- | Parses something in parentheses. This can be a single expression, a
-- tuple, or a block.
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

pLambda :: Expr -> Parser Expr
pLambda param = do
  pExactSym "=>"
  Expr (_pos param) . Lambda param <$> pAnyBlock

pPatternDef :: Expr -> Parser Expr
pPatternDef left = do
  pExactSym "="
  Expr (_pos left) . PatternDef left <$> pExpr

---------------------------------------------------------
-----------------------  Helpers  -----------------------
---------------------------------------------------------

grab :: (PToken -> Maybe a) -> Parser a
grab = token (render ~> unpack) tPosition

satisfy :: (Token PToken -> Bool) -> Parser (Token PToken)
satisfy f = satisfy' (f . tToken)

satisfy' :: (PToken -> Bool) -> Parser (Token PToken)
satisfy' f = grab (\pt -> if f pt then Just (tToken pt) else Nothing)

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

-- | Tokenizes from a given starting position. Returns the ending position.
parseFrom :: SourcePos
          -> [PToken]
          -> Either TokenizerError (Expr, SourcePos)
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
