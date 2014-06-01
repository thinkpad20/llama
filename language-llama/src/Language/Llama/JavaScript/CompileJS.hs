{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Language.Llama.JavaScript.CompileJS (compileIt, jsPreamble) where

import qualified Prelude as P
import qualified Language.Llama.JavaScript.AST as J
import qualified Data.HashMap.Strict as H
import System.IO.Unsafe

import Language.Llama.Common.Common hiding (contains)
import Language.Llama.Common.AST
import Language.Llama.Desugarer.Desugar

data Context = Context {cDeclarations :: [Name]}
type JSCompilerState = [Context]
type JSCompiler = ErrorT ErrorList (StateT JSCompilerState IO)
type CExpr = DExpr

singleE :: J.Expr -> JSCompiler J.Block
singleE e = singleS $ J.Expr e

singleS :: J.Statement -> JSCompiler J.Block
singleS s = return $ J.Block [s]

singleE' :: J.Expr -> J.Block
singleE' e = J.Block [J.Expr e]

-- @eToBlk@ compiles an expression to a block; this means that it will always
-- end with a return statement.
eToBlk :: CExpr -> JSCompiler J.Block
eToBlk expr = case unExpr expr of
  a `Then` b -> do
    a' <- eToE a
    J.Block rest <- eToBlk b
    return $ J.Block (J.Expr a':rest)
  If c t f -> case f of
    Just f -> singleS =<< J.If <$> eToE c <*> eToBlk t <*> (Just <$> eToBlk f)
    Nothing -> singleS =<< J.If <$> eToE c <*> eToBlk t <*> pure Nothing
  Define name e -> do
    let name' = if isSymbol name then toString name else name
    assn <- J.Assign (J.Var name') <$> eToE e
    return $ J.Block [J.Declare [name'], J.Expr assn]
  Throw expr -> singleS =<< J.Throw <$> eToE expr
  e -> singleS =<< J.Return <$> eToE expr
  where
    call e es = singleS $ J.Return $ J.Call e es
    blkToBlk = \case
      [] -> throwErrorC ["Empty block"]
      [e] -> singleS =<< J.Return <$> eToE e
      (e:es) -> mappend <$> compile e <*> blkToBlk es

-- @eToE@ compiles an expression to a JS expression. This is used when we
-- are inside of another expression, or some other situation where we don't
-- need to be producing a full block.
eToE :: DExpr -> JSCompiler J.Expr
eToE expr = case unExpr expr of
  Number n -> return $ J.Number n
  String s -> return $ J.String s
  Attribute e name -> do
    e' <- eToE e
    return $ J.Call (J.Dot e' "get") [J.String name]
  Binary op a b -> do
    (a', b') <- (,) <$> eToE a <*> eToE b
    let op' = J.Var $ toString op
    return $ J.Call (J.Call op' [a']) [b']
  PatAssert pa -> paToE pa
  GetAttrib cName idx e -> do
    e' <- eToE e
    return $ J.Call (J.Dot e' "deref") [J.String cName, J.Number $ fromIntegral idx]
  Constructor "True" -> return $ J.Keyword "true"
  Constructor "False" -> return $ J.Keyword "false"
  Tuple es _ -> jTuple <$> mapM eToE es
  Constructor n -> return $ J.Var $ if isSymbol n then toString n else n
  Var n -> return $ J.Var $ if isSymbol n then toString n else n
  If c t Nothing -> J.Ternary <$> eToE c <*> eToE t <*> pure noElseError
  If c t (Just f) -> J.Ternary <$> eToE c <*> eToE t <*> eToE f
  Lambda pat e -> case unExpr pat of
    Var n -> J.Function [n] <$> eToBlk e
    _ -> throwErrorC ["Pattern ", render pat, " should have been desugared"]
  Throw e -> _throw <$> eToE e
  Apply a b -> J.Call <$> eToE a <*> (pure <$> eToE b)
  Dot b a -> J.Call <$> eToE a <*> (pure <$> eToE b)
  Define name e -> declare name >> J.Assign (J.Var name) <$> eToE e
  otherwise -> throwErrorC ["Unhandlable expression: ", render expr]
  where noElseError = _throwMsg "No else expression"

_throw :: J.Expr -> J.Expr
_throw e = J.Call (J.Var "_throw") [e]

_throwMsg :: Text -> J.Expr
_throwMsg msg = _throw $ J.String msg

jTuple :: [J.Expr] -> J.Expr
jTuple es = J.Call (J.Var "__Tuple") [J.Array es]

paToE :: PatAssert CExpr -> JSCompiler J.Expr
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


iffe :: [DExpr] -> JSCompiler J.Expr
iffe exprs = do
  block <- mconcat <$> mapM eToBlk exprs
  let func = J.Function [] block
  return $ J.Call func []

-- compile is the top-level compilation function. It's almost identical to
-- eToBlk except that it does not produce a return on bare expressions or
-- if statements.
compile :: DExpr -> JSCompiler J.Block
compile expr = case unExpr expr of
  a `Then` b -> do
    a' <- eToE a
    J.Block rest <- compile b
    return $ J.Block (J.Expr a':rest)
  If c t f -> case f of
    Nothing -> singleS =<< J.If <$> eToE c <*> compile t <*> pure Nothing
    Just f -> singleS =<< J.If <$> eToE c <*> compile t <*> (Just <$>compile f)
  Define name e -> do
    expr <- J.Assign (J.Var name) <$> eToE e
    return $ J.Block [J.Declare [name], J.Expr expr]
  Apply a b -> do
    a' <- eToE a
    b' <- eToE b
    call a' [b']
  ObjDec dec -> compileObjectDec dec
  e -> singleE =<< eToE expr
  where call e es = singleE $ J.Call e es

declare _ = return ()

compileObjectDec :: ObjectDec CExpr -> JSCompiler J.Block
compileObjectDec ObjectDec {objConstrs=constrs,
                            objName=name,
                            objAttrs=attrs} =
  let attrs' = map render attrs in
  mconcat <$> mapM (compileConstr name attrs') constrs

compileConstr :: Name -> [Name] -> ConstructorDec CExpr -> JSCompiler J.Block
compileConstr objName attrs c = do
  parent <- case constrExtends c of
    Nothing -> return J.Null
    Just e -> eToE e
  args <- forM (constrArgs c) $ \a -> case unExpr a of
    Var n -> return n
    e -> newName a
  let attrs' = J.Object $ H.fromList (map (\n -> (n,J.Var n)) attrs)
  let new = llamaObj objName (constrName c) args attrs' parent
      mkFunc [] = case attrs of
        [] -> new
        _  -> J.Function attrs (J.Block [J.Return $ new])
      mkFunc (a:as) = J.Function [a] (J.Block [J.Return $ mkFunc as])
      func = J.Expr $ J.Assign (J.Var $ constrName c) (mkFunc $ args)
  return $ J.Block [func]

llamaObj :: Name -- The type's name
         -> Name -- The constructor's name
         -> [Name] -- Names of any arguments
         -> J.Expr -- Object containing attributes
         -> J.Expr -- Parent object, or null
         -> J.Expr
llamaObj objName constrName argNames attrs parent =
  J.New $ J.Call (J.Var "LlamaObject") [J.String objName,
                                        J.String $ constrName,
                                        J.Array $ fmap J.Var argNames,
                                        attrs,
                                        parent]

newName :: DExpr -> JSCompiler Name
newName _ = throwErrorC ["Can't create new names yet"]

contains :: Name -> Type -> Bool
contains n (TVar n') = n == n'
contains n (TApply a b) = contains n a || contains n b
contains n (TFunction a b) = contains n a || contains n b
contains _ _ = False

--gen :: Name -- The name of the bound type variable
--    -> Name -- The name of the trait
--    -> Name -- The name of the function being generated
--    -> Type -- The type of the function being traversed
--    -> JSCompiler J.Expr -- The js function being generated
--gen varName traitName funcName t = go [] t where
--  go :: [Name] -> Type -> JSCompiler J.Expr
--  go argNames (TVar n)
--    | n == varName = makeStub traitName funcName (P.reverse argNames)
--  go argNames t@(TApply a b)
--    | contains varName a = getInstance (P.reverse argNames)
--    | contains varName b = getInstance (P.reverse argNames)
--  go argNames (TFunction a b)
--    | contains varName a = getInstance traitName funcName (P.reverse argNames)
--    | otherwise = makeArgName a >>= \arg -> go (arg:argNames) b
--  go _ _ = throwErrorC ["Type variable `", varName, "` not found in signature"]
--  getInstance args = do
--    argName <- makeArgName varName
--    newFuncName <- makeArgName funcName
--    return $
--      J.Function [argName] $ J.Block [
--          J.Assign (J.Var newFuncName) $
--            J.Call
--              (J.Dot (J.Var traitName) "get_instance")
--              [ J.Var funcName
--              , J.Dot (J.Var argName) "type"]
--        , J.Return $
--            J.Call (J.Var newFuncName) [J.Var argName]
--        ]
--  makeStub = P.undefined
--  makeArgName = P.undefined

runCompile :: DExpr -> IO (Either ErrorList J.Block)
runCompile expr = fst <$> runStateT (runErrorT $ compile expr) []

compileIt :: String -> Either ErrorList J.Block
compileIt input = case desugarIt input of
  Left err -> Left $ ErrorList [render err]
  Right expr -> unsafePerformIO $ runCompile expr

compileIt' :: String -> String
compileIt' input = case compileIt input of
  Left err -> error $ P.show err
  Right js -> unpack $ renderI 2 js

compIt = fmap render . compileIt

compileFile :: FilePath -> Maybe FilePath -> IO ()
compileFile path out = do
  contents <- readFile path
  case compileIt contents of
    Left err -> error $ P.show err
    Right jscript -> do
      let output = unpack $ renderI 0 jscript
      case out of
        Nothing ->  putStrLn output
        Just out -> P.writeFile out output

toString :: Text -> Text
toString = \case
  { ">" -> "_gt"; "<" -> "_lt"; "==" -> "_eq"; ">=" -> "_geq"; "<=" -> "_leq"
  ; "~" -> "_neg"; "+" -> "_add"; "-" -> "_sub"; "*" -> "_mult"; "/" -> "_div"
  ; "&&" -> "_and"; "||" -> "_or"; "^" -> "_pow"; "::" -> "_cons"
  ; "<>" -> "_append"; "!" -> "_f_apply"; "$" -> "_apply"; "~>" -> "_f_comp"
  ; "<~" -> "_comp"; s -> unpack s ! map fromChar ! mconcat ! pack }
  where
    fromChar = \case
      { '>' -> "_gt"; '<' -> "_lt"; '=' -> "_eq"; '~' -> "_tilde"
      ; '+' -> "_plus"; '-' -> "_minus"; '*' -> "_star"; '/' -> "_fslash"
      ; '\\' -> "_bslash"; '&' -> "_amp"; '|' -> "_pipe"; '!' -> "_bang"
      ; '@' -> "_at"; ':' -> "_col"}

jsPreamble = "var ConstructorNotFoundError, Exception, Just, LlamaObject, LookupError, NoAttributeError, Nothing, PatternMatchError, is_instance, print, println, show, _divide, _minus, _plus, _throw, _times;\nLlamaObject = require('./llama_core').LlamaObject;\nis_instance = require('./llama_core').is_instance;\nException = require('./llama_core').Exception;\nLookupError = require('./llama_core').LookupError;\nPatternMatchError = require('./llama_core').PatternMatchError;\nConstructorNotFoundError = require('./llama_core').ConstructorNotFoundError;\nNoAttributeError = require('./llama_core').NoAttributeError;\nNothing = require('./llama_core').Nothing;\nJust = require('./llama_core').Just;\n_throw = require('./llama_core')._throw;\nshow = require('./llama_core').show;\nprintln = require('./llama_core').println;\nprint = require('./llama_core').print;\n_plus = require('./llama_core')._plus;\n_minus = require('./llama_core')._minus;\n_times = require('./llama_core')._times;\n_divide = require('./llama_core')._divide;\n_plus = require('./llama_core')._plus;\n\n"
