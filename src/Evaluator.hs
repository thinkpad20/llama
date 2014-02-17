module Evaluator where

import Common
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
-}

data Value = VNumber Double
           | VString String
           | VFunction Type [Instruction] (M.Map Name Value)
           | VArray [Value]
           | VTuple [Value]
           | VArg ArgRef
           deriving (Show, Eq)

data Instruction = Instruction deriving (Show)

-- | References to as-yet-unset values, as in function args
data ArgRef = ArgRef -- meaning "the argument of this function"
            | Index Int ArgRef -- meaning an index into the argument

type SymbolTable = [M.Map Name Value]
data EvalState = EvalState {table::SymbolTable}
type EvalError = String
type Eval = ErrorT EvalError (StateT EvalState Identity)

push :: Eval ()
push = modify $ \s -> s {table = mempty : table s}
pop :: Eval ()
pop = modify $ \s -> s {table = tail $ table s}
pushWith :: Name -> Value -> Eval ()
pushWith name val = modify $ \s -> s {table = M.singleton name val : table s}

getVarName :: Expr -> Eval Name
getVarName = undefined

typeOf :: Value -> Eval Type
typeOf val = case val of
  VNumber _ -> return $ TConst "Number"
  VString _ -> return $ TConst "String"
  VArray [] -> return $ TConst "[]" `TApply` TVar "a"
  VArray (v:vals) -> do
    (t:types) <- mapM typeOf (v:vals)
    -- later, we'll relax this to allow disparate types unified by interface
    case all (==t) types of
      True -> return $ t:types
      False -> throwError $ "Multiple types found in the same array"
  VTuple vals -> TTuple <$> mapM typeOf vals
  VFunction typ body nmap -> do
    retType <- getReturnType typ body nmap -- catch errors here
                                           -- passing in the type in case
                                           -- of argument references
    return $ TFunction typ retType


eExpr :: Expr -> Eval Value
eExpr expr = case expr of
  Number n -> return $ VNumber n
  String s -> return $ VString s
  Dot a b -> eExpr $ Apply b a
  Apply func arg -> do
    funcVal <- eval func
    -- we should be able to get the type of the argument here
    -- and the type of the function
    -- so we can do type checking right away
    case funcVal of
      Lambda [()]
    argVal <- eval arg
    pushWith varName argVal
    eExpr a <* pop
