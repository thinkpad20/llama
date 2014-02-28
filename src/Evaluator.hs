{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator where

import Prelude hiding (lookup)
import Control.Monad.Reader
import Data.IORef
import System.Console.Haskeline
import System.IO.Unsafe
import qualified Data.Map as M

import Common
import Parser
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
        VLocal ref -> derefArg ref >>= \v -> return (k, v)
        _ -> return (k, v)

derefArg :: LocalRef -> Eval Value
derefArg argRef = do
  arg <- get <!> stack <!> head <!> argument
  go arg argRef
  where
    go :: Value -> LocalRef -> Eval Value
    go arg argRef = case argRef of
      ArgRef -> return arg
      Index n ref -> do
        arg' <- go arg ref
        case arg' of
          VObject _ as | length as > n -> return (as !! n)
          _ -> throwErrorC ["Invalid index `", show n, "' into "
                           , "argument `", render arg, "'"]

lookupAndError name = lookup name >>= \case
  Just (VLocal ref) -> derefArg ref
  Just val-> return val
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

instance Evalable Block where
  eval block = case block of
    [] -> throwError1 "Empty block"
    [stmt] -> eval stmt
    Break expr:_ -> eval expr
    stmt:stmts -> eval stmt >>= \case
      -- this works for now, but we need to thinking about how to propagate
      -- `return`s, how to respond to `break`s, `continue`s, etc.
      VReturn val -> return $ VReturn val
      _ -> eval stmts

instance Evalable Expr where
  eval expr = case expr of
    Number n -> return $ VNumber n
    String s -> return $ VString s
    Dot a b -> eval $ Apply b a
    Var name -> lookupAndError name
    Mut expr -> eval expr >>= newMutable
    Block blk -> eval blk
    Constructor name -> return $ VObject name []
    Define name block -> do
      eval block >>== addLocal name
    Assign dest expr -> eval dest >>= \case
      VMutable ref -> eval expr >>= modMutable ref
      val -> throwErrorC ["Expression `", render dest, "' evaluates to `"
                         , render val, "', which is not a reference." ]
    If' cond true -> eval cond >>= \case
      v | v == trueV -> justV <$> eval true
        | v == falseV -> return nothingV
        | otherwise -> condError
    If cond true false -> eval cond >>= \case
      v | v == trueV -> eval true
        | v == falseV -> eval false
        | otherwise -> condError
    Return expr -> VReturn <$> eval expr
    Apply func arg -> do
      funcVal <- eval func
      -- need to figure out polymorphism here...
      case funcVal of
        VFunction block env -> do
          argVal <- eval arg >>= snapshot
          pushWith StackFrame { argument = argVal, vTable = env }
          eval block <* pop
        VBuiltin (Builtin _ f) ->
          eval arg >>= snapshot >>= f
        _ -> throwErrorC ["`", render func, "' is not a function!"]
    Lambda expr block -> do
      --fixSymTable
      vTable <- computeClosure
      names <- getNames expr
      -- need to store the current argument if there is one
      return $ VFunction block $ M.union names vTable
    Tuple exprs -> vTuple <$> mapM eval exprs
    While cond e -> do
      eval cond >>= \case
        v | v == trueV -> eval e >> eval expr
          | v == falseV -> return nothingV
          | otherwise -> condError
    _ -> throwErrorC ["Can't evaluate `", render expr, "' yet"]

condError = error $ concat ["An if condition must have the type `"
                           , "Bool'; this should have been caught "
                           , "by the type checker"]

getNames :: Expr -> Eval ValueTable
getNames expr = go ArgRef expr where
  go :: LocalRef -> Expr -> Eval ValueTable
  go ref expr = case expr of
    Var name -> return (M.singleton name (VLocal ref))
    Typed (Var name) _ -> return (M.singleton name (VLocal ref))
    Tuple es -> do
      let pairs = zip [0..] es
          go' (index, e) = go (Index index ref) e
      mconcat <$> mapM go' pairs
    _ -> throwErrorC ["Whoopsie, we can't handle `", render expr, "' yet"]


-- NOTE: using unsafePerformIO for testing purposes only
runEvalWith :: Evalable a =>
               EvalState ->
               TypingState ->
               a ->
               (Either ErrorList Value, EvalState)
runEvalWith state tstate a =
  unsafePerformIO $ runStateT (runReaderT (runErrorT $ eval a) tstate) state

runEval :: Evalable a => a -> (Either ErrorList Value, EvalState)
runEval = runEvalWith defaultState defaultTypingState

typeAndEval :: EvalState ->
               TypingState ->
               String ->
               (Either ErrorList Value, EvalState, TypingState)
typeAndEval eState tState input = case grab input of
  Left err ->
    (Left $ TE ["Syntax error:\n" ++ show err], eState, tState)
  Right block ->
    case runTypingWith tState block of
      (Left errs, _) ->
        (Left $ TE ["Type check error:\n" ++ show errs], eState, tState)
      (Right _, tState') ->
        let (result, eState') = runEvalWith eState tState' block in
        (result, eState', tState')


evalIt :: EvalState -> TypingState -> String -> IO (EvalState, TypingState)
evalIt eState tState input = case typeAndEval eState tState input of
  (Left err, _, _) -> do
    print err
    return (eState, tState)
  (Right val, eState, tState) -> do
    renderIO val >>= putStrLn
    return (eState, tState)

repl = do
  putStrLn "Welcome to the Llama REPL."
  runInputT defaultSettings loop'
  where
    loop' = loop defaultState defaultTypingState
    loop eState tState = forever $ do
      getInputLine "llama> " >>= \case
        Nothing -> return ()
        Just "clear" -> do
          lift $ putStrLn "Cleared variables"
          loop defaultState defaultTypingState
        Just input -> do
          (eState', tState') <- lift $ evalIt eState tState input
          loop eState' tState'

main = repl