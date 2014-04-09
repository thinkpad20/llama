{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Evaluator where

import Prelude (IO, Eq(..), Ord(..), Bool(..)
               , Double, Maybe(..), undefined, Monad(..)
               , ($), Int, (.), (*), Either(..), String
               , fst, (+), (-), (/), (=<<), otherwise, fmap)
import qualified Prelude as P
import Control.Monad.Loops

import Common hiding (intercalate)
import AST
import Desugar
import TypeChecker
import EvaluatorLib
import EvaluatorBuiltins
import Data.Sequence (length)


eval :: Expr -> Eval Value
eval = \case
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
  Define name expr -> store name =<< eval expr
  Modified Ref expr -> newRef =<< eval expr
  Assign (Var name) expr -> lookupOrError name >>= \case
    VRef ref -> writeRef ref =<< eval expr
    val -> typeError "Reference" val
  For start cond step expr -> return unitV <* do
    eval start
    whileM_ (checkBool =<< eval cond) $ (eval expr >> eval step)
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
checkBool val = unbox val >>= \case
  VBool b -> return b
  val' -> throwErrorC ["Value `", render val', "' is not a Bool"]

evalLit :: Literal -> Eval Value
evalLit (ArrayLiteral exprs) = VVector . fromList <$> forM exprs eval

evalBlock :: [Expr] -> Eval Value
evalBlock = \case
  [] -> return unitV
  [e] -> eval e
  (e:es) -> eval e >> evalBlock es

evalFunc :: Value -> Value -> Eval Value
evalFunc func arg = case func of
  Closure expr env -> do
    log' ["evaluating a closure: ", render expr]
    e <- renderEnv env
    log' ["env is ", e]
    pushFrame arg env
    result <- eval expr
    popFrame
    log' ["result is ", render result]
    return result
  Builtin (name, bi) -> do
    log' ["Evaluating ", name, " on arg ", render arg]
    result <- bi arg
    log' ["Result is ", render result]
    return result

evalLamda :: Expr -> Expr -> Eval Value
evalLamda param body = Closure body <$> getClosure param body

runEval :: Expr -> IO (Either ErrorList Value, EvalState)
runEval expr = do
  env <- builtIns
  let frame = Frame {eEnv=env, eArg=unitV}
      initState = ES [frame]
  runStateT (runErrorT (eval expr)) initState

evalIt :: String -> IO (Either ErrorList Value)
evalIt input = case desugarIt input of
  Left err -> return (Left $ ErrorList [render err])
  Right expr -> do
    when doTypeChecking (runTyping expr >> return ())
    fst <$> runEval expr >>= \case
      Left err -> return $ Left err
      Right val@(VTuple vec) | length vec == 0 -> return (Right val)
      Right val -> print val >> return (Right val)

evalIt' :: String -> IO ()
evalIt' input = evalIt input >> return ()
