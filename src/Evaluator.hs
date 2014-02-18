{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator where

import System.IO.Unsafe
import Common
import Parser
import AST
import qualified Data.Map as M

{-
In an imperative paradigm, a function is a list of instructions along with
a maping of identifiers to values, with zero of more of those values unset.
So we can know everything we need to know about a function if we have:
  1. A list of instructions [Instruction]
  2. a map from identifiers to values (M.Map Name Value)

  How about args? For example if we have the function
    foo = bar: Number => baz bar
    Then do we need to store that it takes a value of type Num? Or
    can we just look through its map and see that it contains
    {bar: ArgRef (TConst "Number")} and get the type from there?
    Seems good to me.
  How about
    foo = (bar: Number, baz: String) => show bar ++ baz
    Its map would contain { bar: ArgTupleRef 0 (TConst "Number")
                          , baz: ArgTupleRef 1 (TConst "String")}
    So technically it contains all it needs, but it would be cleaner to
    keep the type information in the function... then all we'd need is
    that {bar: ArgTupleRef 0} and since the argument type of foo is a
    (Number, String), we'd know bar: Number. Need more thinking tho.
  How about
    foo = ((bar: Number, baz: String), qux: Number) => bar + baz.length + qux
    `bar` is the 0th index of the 0th index of the argument. So we need a
    recursive method. As in, it's either "the argument itself", or an index
    into an argument reference.
    Then we'd have { bar: Index 0 (Index 0 ArgRef)
                   , baz: Index 1 (Index 0 ArgRef)
                   , qux: Index 1 ArgRef }
    And the arg type of the function would be ((Number, String), Number)

  BUT another view is, we can check types statically, and there's no need to
  keep those types afterwards, because we already will have determined that
  we are passing in the right types!
-}

data Value = VNumber Double
           | VString String
           | VFunction Block SymbolTable
           | VArray [Value]
           | VTuple [Value]
           | VArg ArgRef
           deriving (Show, Eq)

-- | References to as-yet-unset values, as in function args
data ArgRef = ArgRef -- meaning "the argument of this function"
            | Index Int ArgRef -- meaning an index into the argument
            deriving (Show, Eq)

-- might want to abstract out a stack frame?

data StackFrame = StackFrame
  {
    argument :: Value
  , locals   :: [Value]
  , symTable :: SymbolTable
  } deriving (Show)
type SymbolTable = M.Map Name Value
data EvalState = EvalState {stack::[StackFrame]} deriving (Show)
type EvalError = String
type Eval = ErrorT EvalError (StateT EvalState IO)

class Evalable a where
  eval :: a -> Eval Value

-- we probably won't end up using this, but...
defaultFrame :: StackFrame
defaultFrame = StackFrame
  {
    argument = VTuple []
  , locals   = mempty
  , symTable = mempty
  }
defaultState :: EvalState
defaultState = EvalState {stack = []}

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
  let frame' = frame { locals = locals frame ++ [val]
                     , symTable = M.insert name val (symTable frame)}
  modify $ \s -> s {stack = frame' : tail (stack s)}

getStack = get <!> stack
getSymTable = get <!> stack <!> head <!> symTable

instance Evalable Block where
  eval block = last <$> mapM eval block

instance Evalable Statement where
  eval stmt = case stmt of
    Expr expr -> eval expr
    Define (TVar name) block -> do
      eval block >>== addLocal name
    Assign _ _ -> throwError "Assignment not yet supported"
    _ -> error "ruh roh"

instance Evalable Expr where
  eval expr = case expr of
    Number n -> return $ VNumber n
    String s -> return $ VString s
    Dot a b -> eval $ Apply b a
    Apply func arg -> do
      funcVal <- eval func
      -- need to figure out polymorphism here...
      case funcVal of
        VFunction block env -> do
          argVal <- eval arg
          pushWithArg argVal *> eval block <* pop
        _ -> throwError' ["`", render func, "' is not a function!"]
    Lambda [(Typed (Var name) _, block)] -> do
      symTable <- getSymTable
      return $ VFunction block (M.insert name (VArg ArgRef) symTable)
    Lambda _ -> throwError "Lambda alternatives not supported yet"


throwError' = throwError . concat

-- NOTE: using unsafePerformIO for testing purposes only
runEval :: Evalable a => a -> (Either EvalError Value, EvalState)
runEval a = unsafePerformIO $ runStateT (runErrorT $ eval a) defaultState

evalIt :: String -> (Either EvalError Value, EvalState)
evalIt input = case grab input of
  Left err -> error $ show err
  Right block -> runEval block
