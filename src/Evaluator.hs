{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator where

import Prelude hiding (lookup)
import System.IO.Unsafe
import qualified Data.Map as M

import Common
import Parser
import AST
import TypeChecker

data Value = VNumber Double
           | VString String
           | VFunction Expr ValueTable
           | VObject Name [Value]
           | VArray [Value]
           | VArg ArgRef
           deriving (Show, Eq)

-- | References to as-yet-unset values, as in function args
data ArgRef = ArgRef -- meaning "the argument of this function"
            | Index Int ArgRef -- meaning an index into the argument
            deriving (Show, Eq)

data StackFrame = StackFrame
  {
    argument :: Value
  , vTable :: ValueTable
  } deriving (Show)
type ValueTable = M.Map Name Value
data EvalState = EvalState {stack::[StackFrame]
                           , sTable::TypeTable} deriving (Show)
type Eval = ErrorT ErrorList (StateT EvalState IO)

instance Render EvalState where
  render s =  ""

instance Render Value where
  render val = case val of
    VNumber n -> show n
    VString s -> show s
    VFunction res vtable -> render res ++ ", with " ++ render vtable
    VObject "" vals -> "(" ++ (intercalate ", " $ map render vals) ++ ")"
    VObject name vals ->
      name ++ " (" ++ (intercalate ", " $ map render vals) ++ ")"
    VArray vals -> render vals
    VArg aref -> render aref

instance Render ArgRef where
  render aref = case aref of
    ArgRef -> "argument"
    Index i aref -> render aref ++ "[" ++ show i ++ "]"

class Evalable a where
  eval :: a -> Eval Value

vTuple :: [Value] -> Value
vTuple = VObject ""
unitV = vTuple []

-- we probably won't end up using this, but...
defaultFrame :: StackFrame
defaultFrame = StackFrame
  {
    argument = unitV
  , vTable = mempty
  }
defaultState :: EvalState
defaultState = EvalState {stack = [defaultFrame], sTable = mempty}

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

getStack = get <!> stack
getSymTable = get <!> stack <!> head <!> vTable

lookup name = getStack >>= loop
  where loop [] = return Nothing
        loop (frame:frames) = case M.lookup name (vTable frame) of
          Just val -> return (Just val)
          Nothing -> loop frames

lookupAndError name = lookup name >>= \case
  Just (VArg ArgRef) -> get <!> stack <!> head <!> argument
  Just v@(VArg _) -> throwErrorC ["Can't deal with `", render v, "'"]
  Just val-> return val
  Nothing -> throwErrorC ["Variable '", name, "' not defined in scope"]

instance Evalable Block where
  eval block = last <$> mapM eval block

instance Evalable Statement where
  eval stmt = case stmt of
    Expr expr -> eval expr
    Define name block -> do
      eval block >>== addLocal name
    Assign _ _ -> throwError1 "Assignment not yet supported"
    _ -> error "ruh roh"

instance Evalable Expr where
  eval expr = case expr of
    Number n -> return $ VNumber n
    String s -> return $ VString s
    Dot a b -> eval $ Apply b a
    Var name -> lookupAndError name
    Apply func arg -> do
      funcVal <- eval func
      -- need to figure out polymorphism here...
      case funcVal of
        VFunction block env -> do
          argVal <- eval arg
          pushWithArg argVal *> eval block <* pop
        _ -> throwErrorC ["`", render func, "' is not a function!"]
    Lambda expr block -> do
      vTable <- getSymTable
      names <- getNames expr
      return $ VFunction block (M.union names vTable)
    _ -> throwErrorC ["Can't evaluate `", render expr, "' yet"]

getNames :: Expr -> Eval (M.Map Name Value)
getNames expr = case expr of
  Var name -> return (M.singleton name (VArg ArgRef))
  Typed (Var name) _ -> return (M.singleton name (VArg ArgRef))
  _ -> throwErrorC ["Whoopsie, we can't handle `", render expr, "' yet"]


-- NOTE: using unsafePerformIO for testing purposes only
runEvalWith :: Evalable a => EvalState -> a -> (Either ErrorList Value, EvalState)
runEvalWith state a = unsafePerformIO $ runStateT (runErrorT $ eval a) state

runEval :: Evalable a => a -> (Either ErrorList Value, EvalState)
runEval = runEvalWith defaultState

typeAndEval :: String -> (Either ErrorList Value, EvalState)
typeAndEval input = case grab input of
  Left err -> error $ show err
  Right block ->
    case runTyping block of
      (Left errs, _) -> error $ show errs
      (Right _, tstate) ->
        let state = defaultState {sTable = nameSpaceTable tstate} in
        runEvalWith state block

evalIt :: String -> IO ()
evalIt input = case typeAndEval input of
  (Left err, _) -> error $ show err
  (Right val, state) ->
    putStrLn $ render val ++ "\n" ++ render state