{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE BangPatterns #-}
module Evaluator (EvalState, initState, evalIt, evalIt', evalItWith) where

import Prelude (IO, Eq(..), Ord(..), Bool(..), not
               , Double, Maybe(..), undefined, Monad(..)
               , ($), Int, (.), (*), Either(..), String
               , fst, snd, (+), (-), (/), (=<<), otherwise
               , fmap, (&&), (||))
import qualified Prelude as P
import Data.Sequence (length, index)
import qualified Data.Text as T
import Data.IORef

import Common hiding (intercalate)
import AST
import Desugar
import TypeChecker
import EvaluatorLib
import EvaluatorBuiltins


eval :: Expr -> Eval Value
eval !expression = case expression of
  Number n -> return (VNumber n)
  String s -> return (VString s)
  Constructor "True" -> return (VBool True)
  Constructor "False" -> return (VBool False)
  Constructor n -> lookupOrError n
  Var n -> lookupOrError n
  Block exprs -> evalBlock exprs
  Tuple exprs kws | kws == mempty -> do
    vals <- forM exprs eval
    return $ VTuple $ fromList vals
  Tuple _ _ -> throwError1 "tuples with kwargs aren't implemented in evaluator"
  Literal lit -> evalLit lit
  Apply func arg -> do
    log' ["apply, func is ", render func, ", arg is ", render arg]
    funcV <- eval func
    log' ["got a func val ", render funcV]
    argV <- eval arg
    log' ["got an arg val ", render argV]
    evalFunc funcV argV
  Dot arg func -> do
    argV <- eval arg
    funcV <- eval func
    evalFunc funcV argV
  Attribute expr name -> getAttribute name =<< eval expr
  Lambda param body -> evalLamda param body
  Define name expr -> eval expr >>= \case
    Closure Nothing body env -> store name (Closure (Just name) body env)
    val -> store name val
  Modified Ref expr -> newRef =<< eval expr
  Assign (Var name) expr -> lookupOrError name >>= \case
    VRef ref -> writeRef ref =<< eval expr
    val -> typeError "Reference" val
  For start cond step body -> do
    eval start
    loop where
    loop = do
      eval cond >>= checkBool >>= \case
        False -> return nothingV
        True -> eval body >>= \case
          VReturn val -> return (VReturn val)
          VThrow val -> return (VThrow val)
          VBreak val -> return $ justV val
          _ -> eval step >> loop
  TryCatch try options finally -> evalTryCatch try options finally
  If c t f -> eval c >>= checkBool >>= \case
    True -> eval t
    False -> eval f
  If' c t -> eval c >>= checkBool >>= \case
    True -> justV <$> eval t
    False -> return nothingV
  Throw expr -> VThrow <$> eval expr
  Return expr -> VReturn <$> eval expr
  Deref constrName idx expr -> eval expr >>= evalDeref constrName idx
  PatAssert pa -> VBool <$> evalPatAssert pa
  expr -> throwErrorC ["Evaluator can't handle ", render expr]

evalDeref :: Name -> Int -> Value -> Eval Value
evalDeref constrName idx val = case val of
  VInstance inst | constrName == instConstr inst -> indexOf idx inst
                 | otherwise -> case instParent inst of
                   Nothing ->
                     throwError1 "instance's constructor name doesn't match"
                   Just p -> evalDeref constrName idx p
  VTuple vals | length vals > idx -> return $ index vals idx
              | otherwise -> throwError1 "index out of range on tuple"
  VVector vals | length vals > idx -> return $ index vals idx
               | otherwise -> throwError1 "index out of range on vector"
  VString str  | T.length str > idx -> return $ VString $ T.singleton $ T.index str idx
               | otherwise -> throwError1 "index out of range on string"
  VMaybe (Just val) | idx == 0 -> return val
  _ -> throwErrorC [render val, " can't be dereferenced at ", render idx]

evalPatAssert :: PatAssert -> Eval Bool
evalPatAssert (IsConstr name expr) = eval expr >>= \case
  VInstance inst | name == instConstr inst -> return True
  VMaybe (Just _) | name == "Just" -> return True
  VMaybe Nothing | name == "Nothing" -> return True
  _ -> return False
evalPatAssert (e1 `IsLiteral` e2) = do
  (v1, v2) <- (,) <$> eval e1 <*> eval e2
  isEqual v1 v2
evalPatAssert (IsTupleOf size expr) = eval expr >>= \case
  VTuple tup | length tup == size -> return True
  _ -> return False
evalPatAssert (IsVectorOf size expr) = eval expr >>= \case
  VVector vec | length vec == size -> return True
  _ -> return False
evalPatAssert (pa1 `And` pa2) =
  (&&) <$> evalPatAssert pa1 <*> evalPatAssert pa2

evalTryCatch :: Expr -> [(Expr, Expr)] -> Maybe Expr -> Eval Value
evalTryCatch try options finally = eval try >>= \case
  exc@(VThrow _) -> case options of
    [] -> doFinally False exc unitV
    ((_, branch):_) -> doCatch branch exc
  val -> return val
  where
    doCatch branch exc = eval branch >>= doFinally True exc
    doFinally wasCaught exception toReturn = case finally of
      Nothing -> return $ if wasCaught then toReturn else exception
      Just fin -> eval fin >>= \case
        ret@(VReturn _) -> return ret
        _ -> if not wasCaught then return exception else case toReturn of
          ret@(VReturn _) -> return ret
          _ -> return unitV

checkBool :: Value -> Eval Bool
checkBool !val = unbox val >>= \case
  VBool b -> return b
  val' -> throwErrorC ["Value `", render val', "' is not a Bool"]

evalLit :: Literal -> Eval Value
evalLit !(ArrayLiteral exprs) = VVector . fromList <$> forM exprs eval

evalBlock :: [Expr] -> Eval Value
evalBlock !exprs = case exprs of
  [] -> return unitV
  [e] -> eval e
  (e:es) -> eval e >>= \case
    VReturn val -> return val
    VThrow val -> return (VThrow val)
    _ -> evalBlock es

evalFunc :: Value -> Value -> Eval Value
evalFunc !func !arg = case func of
  Closure n expr env -> do
    let name = case n of {Just nm -> nm; Nothing -> "anonymous function"}
    pushFrame name arg env *> (eval expr >>= unReturn) <* popFrame
  Builtin (_, bi) -> bi arg

unReturn :: Value -> Eval Value
unReturn (VReturn val) = return val
unReturn val = return val

evalLamda :: Expr -> Expr -> Eval Value
evalLamda !param !body = Closure Nothing body <$> getClosure param body

initState :: IO EvalState
initState = do
  !env <- builtIns
  !ref <- newIORef 0
  let frame = Frame {eEnv=env, eArg=unitV, eTrace=defSTE}
  return ES {esStack=[frame], esInstrCount=ref}

runEvalWith :: EvalState -> Expr -> IO (Either ErrorList Value, EvalState)
runEvalWith state expr = runStateT (runErrorT (eval expr)) state

evalIt :: String -> IO (Either ErrorList Value, EvalState)
evalIt !input = do
  state <- initState
  evalItWith state input

evalIt' :: String -> IO ()
evalIt' !input = evalIt input >> return ()

evalItWith :: EvalState -> String -> IO (Either ErrorList Value, EvalState)
evalItWith !state !input = case desugarIt input of
  Left err -> return (Left $ ErrorList [render err], state)
  Right expr -> do
    when doTypeChecking (runTyping expr >> return ())
    runEvalWith state expr >>= \case
      r@(Left err, _) -> do
        putStrLn "Error:"
        print err
        return r
      (res, state') -> case res of
        Right val@(VTuple tup) | length tup == 0 -> finish val
        Right val -> print val >> finish val
        where
          finish val = do
            let ref = esInstrCount state'
            count <- readIORef ref
            putStrLn $ "Evaluated " <> show count <> " expressions"
            return (Right val, state')
