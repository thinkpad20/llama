{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module EvaluatorLib where

import Control.Monad.Reader
import qualified Data.Map as M

import Common
import Parser
import AST
import TypeChecker

data Value = VNumber Double
           | VString String
           | VFunction Expr ValueTable
           | VBuiltin Builtin
           | VObject Name [Value]
           | VArray [Value]
           | VArg ArgRef
           | VReturn Value
           | VThrow Value
           deriving (Show, Eq)

data Builtin = Builtin Name (Value -> Eval Value)
instance Show Builtin where
  show (Builtin n _) = "(BUILTIN/" ++ n ++")"
instance Eq Builtin where
  (Builtin n _) == (Builtin n' _) = n == n'

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
type Eval' = ErrorT ErrorList (ReaderT TypingState (StateT EvalState IO))


instance Render EvalState where
  render s =  ""

instance Render Value where
  render val = case val of
    VNumber n | isInt n -> show $ floor n
    VNumber n -> show n
    VString s -> show s
    VFunction res vtable -> render res ++ ", with " ++ render vtable
    VObject "" vals -> "(" ++ (intercalate ", " $ map render vals) ++ ")"
    VObject name vals ->
      name ++ " (" ++ (intercalate ", " $ map render vals) ++ ")"
    VArray vals -> render vals
    VArg aref -> render aref
    VBuiltin bi -> show bi
    VReturn val -> render val
    VThrow val -> "throw " ++ render val

instance Render ArgRef where
  render aref = case aref of
    ArgRef -> "argument"
    Index i aref -> render aref ++ "[" ++ show i ++ "]"

class Evalable a where
  eval :: a -> Eval Value

vTuple :: [Value] -> Value
vTuple = VObject ""
unitV = vTuple []
trueV = VObject "True" []
falseV = VObject "False" []

-- we probably won't end up using this, but...
defaultFrame :: StackFrame
defaultFrame = StackFrame
  {
    argument = unitV
  , vTable = builtins
  }
defaultState :: EvalState
defaultState = EvalState {stack = [defaultFrame], sTable = mempty}

------------- BUILTIN FUNCTIONS --------------
illegalArgErr name arg = throwErrorC $
  ["Illegal arguments to BUILTIN/", name, ": `", render arg, "'"]

numNumOp :: (Double -> Double -> Double) -> Name -> Builtin
numNumOp op name = Builtin name $ \arg -> case arg of
  VObject "" [VNumber n, VNumber m] -> return $ VNumber (op n m)
  _ -> illegalArgErr name arg

numBoolOp :: (Double -> Double -> Bool) -> Name -> Builtin
numBoolOp op name = Builtin name $ \arg -> case arg of
  VObject "" [VNumber n, VNumber m] -> return $
    if op n m then trueV else falseV
  _ -> illegalArgErr name arg

addNumber, subtractNumber, multiplyNumber, divideNumber :: Builtin
addNumber = numNumOp (+) "addNumber"
subtractNumber = numNumOp (-) "subtractNumber"
multiplyNumber = numNumOp (*) "multiplyNumber"
divideNumber = numNumOp (/) "divideNumber"
ltNumber = numBoolOp (<) "ltNumber"
gtNumber = numBoolOp (>) "gtNumber"
leqNumber = numBoolOp (<=) "leqNumber"
geqNumber = numBoolOp (>=) "geqNumber"
eqNumber = numBoolOp (==) "eqNumber"
neqNumber = numBoolOp (/=) "neqNumber"

printVal :: Builtin
printVal = Builtin "genericPrint" $ \val -> do
  case val of
    VString s -> lift $ lift $ putStrLn s
    val -> lift $ lift $ putStrLn $ render val
  return unitV

printStr :: Builtin
printStr = Builtin "printStr" $ \case
  VString str -> do lift $ lift $ putStrLn str
                    return unitV
  val -> illegalArgErr "printStr" val

lApply, rApply, lComp, rComp :: Value
lApply = VFunction body mp where
  mp = M.fromList [ ("f", VArg (Index 0 ArgRef))
                  , ("x", VArg (Index 1 ArgRef))]
  body = Apply (Var "f") (Var "x")
rApply = VFunction body mp where
  mp = M.fromList [ ("x", VArg (Index 0 ArgRef))
                  , ("f", VArg (Index 1 ArgRef))]
  body = Apply (Var "f") (Var "x")
lComp = VFunction body mp where
  mp = M.fromList [ ("f", VArg (Index 0 ArgRef))
                  , ("g", VArg (Index 1 ArgRef))]
  body = Lambda (Var "x")
           $ Apply (Var "f") (Apply (Var "g") (Var "x"))
rComp = VFunction body mp where
  mp = M.fromList [ ("g", VArg (Index 0 ArgRef))
                  , ("f", VArg (Index 1 ArgRef))]
  body = Lambda (Var "x")
           $ Apply (Var "f") (Apply (Var "g") (Var "x"))

builtins = M.fromList
  [ ("+", VBuiltin addNumber), ("-", VBuiltin subtractNumber)
  , ("*", VBuiltin multiplyNumber), ("/", VBuiltin divideNumber)
  , ("<", VBuiltin ltNumber), (">", VBuiltin gtNumber)
  , ("<=", VBuiltin leqNumber), (">=", VBuiltin geqNumber)
  , ("==", VBuiltin eqNumber), ("!=", VBuiltin neqNumber)
  , ("<|", lApply), ("|>", rApply)
  , ("<~", lComp), ("~>", rComp)
  , ("print", VBuiltin printVal), ("print(Str)", VBuiltin printVal)
  ]