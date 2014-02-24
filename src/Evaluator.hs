{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator where

import Prelude hiding (lookup)
import System.Console.Haskeline
import System.IO.Unsafe
import System.IO
import qualified Data.Map as M

import Common
import Parser
import AST
import TypeChecker
import EvaluatorLib

push :: Eval ()
push = modify $ \s -> s {stack = defaultFrame : stack s}
pop :: Eval ()
pop = modify $ \s -> s {stack = tail $ stack s}
pushWith :: StackFrame -> Eval ()
pushWith frame = modify $ \s -> s {stack = frame : stack s}
pushWithArg :: Value -> Eval ()
pushWithArg arg = pushWith defaultFrame{argument = arg}

addLocal :: Name -> Value -> Eval ()
addLocal name val = do
  frame <- head <$> getStack
  let frame' = frame { vTable = M.insert name val (vTable frame) }
  modify $ \s -> s {stack = frame' : tail (stack s)}

getStack :: Eval [StackFrame]
getStack = get <!> stack
--getSymTable = get <!> stack <!> head <!> vTable

lookup :: Name -> Eval (Maybe Value)
lookup name = getStack >>= loop
  where loop [] = return Nothing
        loop (frame:frames) = case M.lookup name (vTable frame) of
          Just val -> return (Just val)
          Nothing -> loop frames

computeClosure :: Eval ValueTable
computeClosure = do
  stack <- getStack
  case stack of
    [] -> return mempty
    s:_ -> fmap M.fromList $ forM (M.toList $ vTable s) $ \(k, v) -> do
      case v of
        VArg ref -> derefArg ref >>= \v -> return (k, v)
        _ -> return (k, v)

derefArg :: ArgRef -> Eval Value
derefArg argRef = do
  arg <- get <!> stack <!> head <!> argument
  go arg argRef
  where
    go :: Value -> ArgRef -> Eval Value
    go arg argRef = case argRef of
      ArgRef -> return arg
      Index n ref -> do
        arg' <- go arg ref
        case arg' of
          VObject _ as | length as > n -> return (as !! n)
          _ -> throwErrorC ["Invalid index `", show n, "' into "
                           , "argument `", render arg, "'"]

lookupAndError name = lookup name >>= \case
  Just (VArg ref) -> derefArg ref
  Just val-> return val
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

instance Evalable Block where
  eval block = case block of
    [] -> throwError1 "Empty block"
    [stmt] -> eval stmt
    Break:_ -> return unitV
    stmt:stmts -> eval stmt >>= \case
      -- this works for now, but we need to thinking about how to propagate
      -- `return`s, how to respond to `break`s, `continue`s, etc.
      VReturn val -> return $ VReturn val
      _ -> eval stmts

instance Evalable Statement where
  eval stmt = case stmt of
    Expr expr -> eval expr
    Define name block -> do
      eval block >>== addLocal name
    Assign _ _ -> throwError1 "Assignment not yet supported"
    If cond true false -> do
      condV <- eval cond
      case condV of
        v | v == trueV -> eval true
        v | v == falseV -> eval false
        _ -> error $ concat ["An if condition must have the type `"
                            , "Bool'; this should have been caught "
                            , "by the type checker"]
    Return expr -> VReturn <$> eval expr
    _ -> throwErrorC ["We can't handle statement `", render stmt, "'"]

instance Evalable Expr where
  eval expr = case expr of
    Number n -> return $ VNumber n
    String s -> return $ VString s
    Dot a b -> eval $ Apply b a
    Var name -> lookupAndError name
    Block blk -> eval blk
    --Apply (Var name) arg -> do
      -- type <- look up arg's type
      -- look up (name ++ filter (notElem " \n\t") (show type))
    Apply func arg -> do
      funcVal <- eval func
      -- need to figure out polymorphism here...
      case funcVal of
        VFunction block env -> do
          argVal <- eval arg
          pushWith StackFrame { argument = argVal, vTable = env }
          eval block <* pop
        VBuiltin (Builtin _ f) -> eval arg >>= f
        _ -> throwErrorC ["`", render func, "' is not a function!"]
    Lambda expr block -> do
      --fixSymTable
      vTable <- computeClosure
      names <- getNames expr
      -- need to store the current argument if there is one
      return $ VFunction block $ M.union names vTable
    Tuple exprs -> vTuple <$> mapM eval exprs
    _ -> throwErrorC ["Can't evaluate `", render expr, "' yet"]

getNames :: Expr -> Eval ValueTable
getNames expr = go ArgRef expr where
  go :: ArgRef -> Expr -> Eval ValueTable
  go ref expr = case expr of
    Var name -> return (M.singleton name (VArg ref))
    Typed (Var name) _ -> return (M.singleton name (VArg ref))
    Tuple es -> do
      let pairs = zip [0..] es
          go' (index, e) = go (Index index ref) e
      mconcat <$> mapM go' pairs
    _ -> throwErrorC ["Whoopsie, we can't handle `", render expr, "' yet"]


-- NOTE: using unsafePerformIO for testing purposes only
runEvalWith :: Evalable a => EvalState -> a -> (Either ErrorList Value, EvalState)
runEvalWith state a = unsafePerformIO $ runStateT (runErrorT $ eval a) state

runEval :: Evalable a => a -> (Either ErrorList Value, EvalState)
runEval = runEvalWith defaultState

typeAndEval :: String -> (Either ErrorList Value, EvalState, TypingState)
typeAndEval input = case grab input of
  Left err ->
    ( Left $ TE ["Syntax error:\n" ++ show err]
    , defaultState, defaultTypingState)
  Right block ->
    case runTyping block of
      (Left errs, _) ->
        ( Left $ TE ["Type check error:\n" ++ show errs]
        , defaultState, defaultTypingState)
      (Right _, tState) ->
        let state = defaultState {sTable = nameSpaceTable tState}
            (result, eState) = runEvalWith state block in
        (result, eState, tState)


evalIt :: String -> IO ()
evalIt input = case typeAndEval input of
  (Left err, _, _) -> print err
  (Right val, eState, tState) ->
    putStrLn $ render val ++ "\n" ++ render eState

repl = runInputT defaultSettings loop where
  loop = forever $ do
    getInputLine "> " >>= \case
      Nothing -> return ()
      Just input -> lift $ evalIt input

main = repl