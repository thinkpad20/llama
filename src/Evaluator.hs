{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE BangPatterns #-}
module Evaluator (EvalState, initState, evalIt, evalIt', evalItWith) where

import Prelude (IO, Eq(..), Ord(..), Bool(..)
               , Double, Maybe(..), undefined, Monad(..)
               , ($), Int, (.), (*), Either(..), String
               , fst, snd, (+), (-), (/), (=<<), otherwise, fmap)
import qualified Prelude as P
import Data.Sequence (length)
import Data.IORef
import Data.Text hiding (length)

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
        False -> return unitV
        True -> eval body >>= \case
          VReturn val -> return (VReturn val)
          VBreak _ -> return unitV
          _ -> eval step >> loop
  If c t f -> eval c >>= \case
    VBool True -> eval t
    VBool False -> eval f
    val -> throwErrorC ["Value `", render val, "' is not of type Bool."]
  If' c t -> eval c >>= \case
    VBool True -> justV =<< eval t
    VBool False -> return nothingV
    val -> throwErrorC ["Value `", render val, "' is not of type Bool."]
  expr -> throwErrorC ["Evaluator can't handle ", render expr]

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
  (e:es) -> eval e >> evalBlock es

evalFunc :: Value -> Value -> Eval Value
evalFunc !func !arg = case func of
  Closure n expr env -> do
    let name = case n of {Just nm -> nm; Nothing -> "anonymous function"}
    pushFrame name arg env *> eval expr <* popFrame
  Builtin (_, bi) -> bi arg

evalLamda :: Expr -> Expr -> Eval Value
evalLamda !param !body = Closure Nothing body <$> getClosure param body

initState :: IO EvalState
initState = do
  !env <- builtIns
  !ref <- newIORef 0
  let frame = Frame {eEnv=env, eArg=unitV, eTrace=defSTE}
  return ES {esStack=[frame], esInstrCount=ref}

runEval :: Expr -> IO (Either ErrorList Value, EvalState)
runEval expr = initState >>= \s -> runEvalWith s expr

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
