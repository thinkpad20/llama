{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CompileJS where

import qualified JavaScript.AST as J
import Data.Monoid
import Data.Text hiding (map, foldr)
import Control.Applicative
import Data.Char (toLower)
import System.IO.Unsafe

import Common
import AST
import Desugar

data Context = Context {
    cDeclarations :: [Name]
  }
type JSCompilerState = [Context]
type JSCompiler = ErrorT ErrorList (StateT JSCompilerState IO)

singleE :: J.Expr -> JSCompiler J.Block
singleE e = singleS $ J.Expr e

singleS :: J.Statement -> JSCompiler J.Block
singleS s = return $ J.Block [] [s]

singleE' :: J.Expr -> J.Block
singleE' e = J.Block [] [J.Expr e]

-- eToBlk compiles an expression to a block; this means that it will always
-- end with a return statement.
eToBlk :: Expr -> JSCompiler J.Block
eToBlk expr = case expr of
  If c t f -> singleS =<< J.If <$> eToE c <*> eToBlk t <*> eToBlk f
  Define name e -> do
    let name' = if isSymbol name then toString name else name
    assn <- J.Assign (J.Var name') <$> eToE e
    return $ J.Block [name'] [J.Expr assn]
  Apply a b -> do
    a' <- eToE a
    b' <- eToE b
    call a' [b']
  Block es -> blkToBlk es
  Throw expr -> singleS =<< J.Throw <$> eToE expr
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
  Attribute e name -> do
    e' <- eToE e
    return $ J.Call (J.Dot e' "get") [J.String name]
  PatAssert pa -> paToE pa
  Deref cName idx e -> do
    e' <- eToE e
    return $ J.Call (J.Dot e' "deref") [J.String cName, J.Number $ fromIntegral idx]
  Constructor "True" -> return $ J.Bool True
  Constructor "False" -> return $ J.Bool False
  Tuple es _ -> jTuple <$> mapM eToE es
  Constructor n -> return $ J.Var $ if isSymbol n then toString n else n
  Var n -> return $ J.Var $ if isSymbol n then toString n else n
  If c t f -> J.Ternary <$> eToE c <*> eToE t <*> eToE f
  Lambda (Var n) e -> J.Function [n] <$> eToBlk e
  Lambda (Tuple exprs _) e -> J.Function <$> mapM toArg exprs <*> eToBlk e
    where toArg (Var x) = return x
          toArg _ = throwErrorC ["Can't handle arbitrary exprs in args"]
  Block exprs -> iffe exprs
  Throw e -> J.Call (J.Var "_throw") . pure <$> eToE e
  Apply a (Tuple es _) -> J.Call <$> eToE a <*> mapM eToE es
  Apply (Apply (Var op) x) y | op `elem` ["+", "-", "*"] ->
    J.Binary op <$> eToE y <*> eToE y
  Apply a b -> J.Call <$> eToE a <*> (pure <$> eToE b)
  Dot b a -> J.Call <$> eToE a <*> (pure <$> eToE b)
  Define name e -> declare name >> J.Assign (J.Var name) <$> eToE e
  Assign a b -> J.Assign <$> eToE a <*> eToE b
  otherwise -> throwErrorC ["Unhandlable expression: ", render expr]

paToE :: PatAssert -> JSCompiler J.Expr
paToE = \case
  IsLiteral a b -> J.Binary "===" <$> eToE a <*> eToE b
  IsConstr name expr -> do
    expr' <- eToE expr
    return $ J.Binary "===" (J.Dot expr' "___constr_name") (J.String name)
  IsTupleOf size expr -> do
    expr' <- eToE expr
    let isTuple = J.Binary "===" (J.Dot expr' "___constr_name") (J.String "__Tuple")
        val = J.Dot expr' "___values"
    let isSize = J.Binary "===" (J.Dot val "length") (J.Number (fromIntegral size))
    return $ J.Binary "&&" isTuple isSize
  IsArrayOf size expr -> do
    expr' <- eToE expr
    let isArray = J.Call (J.Var "__is_array") [expr']
    let isSize = J.Binary "===" (J.Dot expr' "length") (J.Number (fromIntegral size))
    return $ J.Binary "&&" isArray isSize
  pa1 `And` pa2 -> J.Binary "&&" <$> paToE pa1 <*> paToE pa2


iffe :: [Expr] -> JSCompiler J.Expr
iffe exprs = do
  block <- mconcat <$> mapM eToBlk exprs
  let func = J.Function [] block
  return $ J.Call func []

jTuple :: [J.Expr] -> J.Expr
jTuple es = J.Call (J.Var "__Tuple") [J.Array es]

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
  Apply a b -> do
    a' <- eToE a
    b' <- eToE b
    call a' [b']
  ObjDec dec -> compileObjectDec dec
  e -> singleE =<< eToE e
  where call e es = singleE $ J.Call e es

declare _ = return ()

compileObjectDec :: ObjectDec -> JSCompiler J.Block
compileObjectDec ObjectDec {objConstrs=constrs, objName=name} =
  mconcat <$> mapM (compileConstr name) constrs

compileConstr :: Name -> ConstructorDec -> JSCompiler J.Block
compileConstr objName c = do
  parent <- case constrExtends c of
    Nothing -> return J.Null
    Just e -> eToE e
  logic <- case constrLogic c of
    Nothing -> return J.Null
    Just e -> throwErrorC ["Can't do constructor logic yet"]
  args <- forM (constrArgs c) $ \case
    Var n -> return n
    e -> newName e
  let new = llamaObj objName (constrName c) args J.emptyObj parent logic
  let mkFunc [] = new
      mkFunc (a:as) = J.Function [a] (J.Block [] [J.Return $ mkFunc as])
  let func = J.Expr $ J.Assign (J.Var $ constrName c) (mkFunc args)
  return $ J.Block [constrName c] [func]

llamaObj :: Name -- The type's name
         -> Name -- The constructor's name
         -> [Name] -- Names of any arguments
         -> J.Expr -- Object containing attributes
         -> J.Expr -- Parent object, or null
         -> J.Expr -- Logic to run when instantiated
         -> J.Expr
llamaObj objName constrName argNames attrs parent logic =
  J.New $ J.Call (J.Var "LlamaObject") [J.String objName,
                                        J.String $ constrName,
                                        J.Array $ fmap J.Var argNames,
                                        attrs,
                                        parent,
                                        logic]

newName :: Expr -> JSCompiler Name
newName _ = throwErrorC ["Can't create new names yet"]

runCompile :: Expr -> IO (Either ErrorList J.Block)
runCompile expr = fst <$> runStateT (runErrorT $ compile expr) []

compileIt :: String -> Either ErrorList J.Block
compileIt input = case desugarIt input of
  Left err -> Left $ ErrorList [render err]
  Right expr -> unsafePerformIO $ runCompile expr

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

