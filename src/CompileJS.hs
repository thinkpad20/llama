{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CompileJS where

import qualified JavaScript.AST as J
import Data.Monoid
import Data.Text hiding (map)
import Control.Applicative
import Data.Char (toLower)
import Prelude hiding (foldr)

import Common
import AST
import Desugar

data Context = Context {
    cDeclarations :: [Name]
  }
type JSCompilerState = [Context]
type JSCompiler = ErrorT ErrorList (State JSCompilerState)

-- Exporting functions
--toJs :: String -> IO J.Block
--toJs src = grab src >>= return . compile

--renderJS :: String -> IO String
--renderJS src = toJs src >>= return . (render 0)

--ppJS :: String -> IO ()
--ppJS src = renderJS src >>= putStrLn

singleE :: J.Expr -> JSCompiler J.Block
singleE e = singleS $ J.Expr e

singleS :: J.Statement -> JSCompiler J.Block
singleS s = return $ J.Block [] [s]

--makeConstructors :: Name -> [Constructor] -> JSCompiler J.Block
--makeConstructors name cs = mconcat (construct ~> single <$> cs) where
--  tName (TVar _ v) = v
--  tName (TConst "[]") = "list"
--  tName (TConst c) = map toLower c
--  tName (TTuple ts) = concatMap tName ts
--  tName (a :=> b) = tName a ++ "to" ++ tName b
--  tName (TApply a b) = tName a ++ tName b
--  mkNames ts = zipWith (++) (tName <$> ts) (show <$> [0..])
--  construct :: Constructor -> J.Statement
--  construct (Constructor name types) = case types of
--    [] -> J.Assign (mkVName name) (J.Array [J.String name])
--    ts -> J.Assign (mkVName name) $ func (mkNames ts) where
--      vars = J.Var <$> mkNames ts
--      func [] = J.Array (J.String name : vars)
--      func (name:names) = J.Function [name] $ single $ J.Return $ func names
--  mkVName "[]" = J.Var "Empty"
--  mkVName name = J.Var $ if isSymbol name then toString name else name


-- Compilation
-- eToBlk compiles an expression to a block; this means that it will always
-- end with a return statement.
eToBlk :: Expr -> JSCompiler J.Block
eToBlk expr = case expr of
  If c t f -> singleS =<< J.If <$> eToE c <*> eToBlk t <*> eToBlk f
  Define name e -> do
    let name' = if isSymbol name then toString name else name
    assn <- J.Assign (J.Var name') <$> eToE e
    return $ J.Block [name'] [J.Expr assn]
  Apply a (Tuple es _) -> do
    a' <- eToE a
    es' <- mapM eToE es
    call a' es'
  Apply a b -> do
    a' <- eToE a
    b' <- eToE b
    call a' [b']
  Block es -> blkToBlk es
  e -> singleS =<< J.Return <$> eToE e
  where
    call e es = singleS $ J.Return $ J.Call e es
    blkToBlk = \case
      [] -> throwErrorC ["Empty block"]
      [e] -> singleS =<< J.Return <$> eToE e
      (e:es) -> mappend <$> compile e <*> blkToBlk es

-- eToE compiles an expression to a JS expression. This is used when we
-- are inside of another expression, or some other situation where we don't
-- need to be producing a full block.
eToE :: Expr -> JSCompiler J.Expr
eToE expr = case expr of
  Number n -> return $ J.Number n
  String s -> return $ J.String s
  Constructor "True" -> return $ J.Bool True
  Constructor "False" -> return $ J.Bool False
  Tuple es _ -> J.Array <$> mapM eToE es
  Constructor n -> return $ J.Var $ if isSymbol n then toString n else n
  Var n -> return $ J.Var $ if isSymbol n then toString n else n
  If c t f -> J.Ternary <$> eToE c <*> eToE t <*> eToE f
  Lambda (Var n) e -> J.Function [n] <$> eToBlk e
  Lambda (Tuple exprs _) e -> J.Function <$> mapM toArg exprs <*> eToBlk e
    where toArg (Var x) = return x
          toArg _ = throwErrorC ["Can't handle arbitrary exprs in args"]
  Block exprs -> iffe exprs
  Apply a (Tuple es _) -> J.Call <$> eToE a <*> mapM eToE es
  Apply a b -> J.Call <$> eToE a <*> (pure <$> eToE b)
  Dot b a -> J.Call <$> eToE a <*> (pure <$> eToE b)
  Define name e -> declare name >> J.Assign (J.Var name) <$> eToE e
  Assign a b -> J.Assign <$> eToE a <*> eToE b
  otherwise -> throwErrorC ["Unhandlable expression: ", render expr]

iffe :: [Expr] -> JSCompiler J.Expr
iffe exprs = do
  block <- mconcat <$> mapM eToBlk exprs
  let func = J.Function [] block
  return $ J.Call func []

-- compile is the top-level compilation function. It's almost identical to
-- eToBlk except that it does not produce a return on bare expressions or
-- if statements.
compile :: Expr -> JSCompiler J.Block
compile expr = case expr of
  Block exprs -> mconcat <$> mapM compile exprs
  If c t f -> singleS =<< J.If <$> eToE c <*> compile t <*> compile f
  If' c t -> singleS =<< J.If' <$> eToE c <*> compile t
  Define name e -> do
    expr <- J.Assign (J.Var name) <$> eToE e
    return $ J.Block [name] [J.Expr expr]
  Apply a (Tuple es _) -> do
    a' <- eToE a
    es' <- mapM eToE es
    call a' es'
  Apply a b -> do
    a' <- eToE a
    b' <- eToE b
    call a' [b']
  ObjDec dec -> compileObjectDec dec
  e -> singleE =<< eToE e
  where call e es = singleE $ J.Call e es

declare _ = return ()
compileObjectDec = undefined

runCompile :: Expr -> Either ErrorList J.Block
runCompile expr = evalState (runErrorT $ compile expr) []

compileIt :: String -> Either ErrorList J.Block
compileIt input = case desugarIt input of
  Left err -> Left $ ErrorList [render err]
  Right expr -> runCompile expr

toString :: Text -> Text
toString ">" = "_gt"
toString "<" = "_lt"
toString "==" = "_eq"
toString ">=" = "_geq"
toString "<=" = "_leq"
toString "~" = "_neg"
toString "+" = "_add"
toString "-" = "_sub"
toString "*" = "_mult"
toString "/" = "_div"
toString "&&" = "_and"
toString "||" = "_or"
toString "^" = "_pow"
toString "::" = "_cons"
toString "<>" = "_append"
toString s = unpack s ! map fromChar ! mconcat ! pack where
  fromChar '>' = "_gt"
  fromChar '<' = "_lt"
  fromChar '=' = "_eq"
  fromChar '~' = "_tilde"
  fromChar '+' = "_plus"
  fromChar '-' = "_minus"
  fromChar '*' = "_star"
  fromChar '/' = "_fslash"
  fromChar '\\' = "_bslash"
  fromChar '&' = "_amp"
  fromChar '|' = "_pipe"
  fromChar '!' = "_bang"
  fromChar '@' = "_at"
  fromChar ':' = "_col"

