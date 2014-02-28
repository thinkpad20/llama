{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module EvaluatorLib where

import Prelude hiding (log)
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M

import Common
import Parser
import TypeChecker

data Value = VNumber Double
           | VString String
           | VFunction Expr ValueTable
           | VBuiltin Builtin
           | VObject Name [Value]
           | VArray [Value]
           | VLocal LocalRef
           | VMutable (IORef Value)
           | VReturn Value
           | VThrow Value
           deriving (Show, Eq)

data Builtin = Builtin Name (Value -> Eval Value)

instance Show (IORef Value) where
  show _ = "(mutable value)"
instance Show Builtin where
  show (Builtin n _) = "(BUILTIN/" ++ n ++ ")"
instance Eq Builtin where
  (Builtin n _) == (Builtin n' _) = n == n'

-- | References to as-yet-unset values, as in function args
data LocalRef = ArgRef -- meaning "the argument of this function"
              | Index Int LocalRef -- meaning an index into the argument
              deriving (Show, Eq)

data StackFrame = StackFrame
  {
    argument :: Value
  , vTable :: ValueTable
  } deriving (Show)
type ValueTable = M.Map Name Value
data EvalState = EvalState {stack::[StackFrame]} deriving (Show)
--type Eval = ErrorT ErrorList (StateT EvalState IO)
type Eval = ErrorT ErrorList (ReaderT TypingState (StateT EvalState IO))

instance Render StackFrame where
  render frame = line $ concat ["Argument: ", render (argument frame)
                               , ", Names: ", render tbl'] where
    tbl' = M.filterWithKey (\k _ -> M.notMember k builtins) (vTable frame)

instance Render EvalState where
  render s = render $ stack s

instance Render Value where
  render val = case val of
    VNumber n | isInt n -> show $ floor n
    VNumber n -> show n
    VString s -> show s
    VFunction res vtable -> render res ++ ", with " ++ render vtable'
      where vtable' = M.filterWithKey (\k _ -> M.notMember k builtins) vtable
    VObject "" vals -> "(" ++ (intercalate ", " $ map render vals) ++ ")"
    VObject name vals ->
      name ++ " (" ++ (intercalate ", " $ map render vals) ++ ")"
    VArray vals -> render vals
    VLocal local -> render local
    VBuiltin bi -> show bi
    VReturn val -> render val
    VThrow val -> "throw " ++ render val
    VMutable ref -> "(mutable reference)"
  renderIO val = case val of
    VMutable ref -> render <$> readIORef ref
    val -> return $ render val

instance Render LocalRef where
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
defaultState = EvalState {stack = [defaultFrame]}

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
    VString s -> lift $ lift $ lift $ putStrLn s
    val -> lift $ lift $ lift $ putStrLn $ render val
  return unitV

printStr :: Builtin
printStr = Builtin "printStr" $ \case
  VString str -> do lift $ lift $ lift $ putStrLn str
                    return unitV
  val -> illegalArgErr "printStr" val

lApply, rApply, lComp, rComp :: Value
lApply = VFunction body mp where
  mp = M.fromList [ ("f", VLocal (Index 0 ArgRef))
                  , ("x", VLocal (Index 1 ArgRef))]
  body = Apply (Var "f") (Var "x")
rApply = VFunction body mp where
  mp = M.fromList [ ("x", VLocal (Index 0 ArgRef))
                  , ("f", VLocal (Index 1 ArgRef))]
  body = Apply (Var "f") (Var "x")
lComp = VFunction body mp where
  mp = M.fromList [ ("f", VLocal (Index 0 ArgRef))
                  , ("g", VLocal (Index 1 ArgRef))]
  body = Lambda (Var "x")
           $ Apply (Var "f") (Apply (Var "g") (Var "x"))
rComp = VFunction body mp where
  mp = M.fromList [ ("g", VLocal (Index 0 ArgRef))
                  , ("f", VLocal (Index 1 ArgRef))]
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


newMutable :: Value -> Eval Value
newMutable val = fmap VMutable $ lift3 $ newIORef val

modMutable :: (IORef Value) -> Value -> Eval Value
modMutable ref val = do
  lift3 $ writeIORef ref val
  return val

-- | Converts any mutable variables to immutables
snapshot :: Value -> Eval Value
snapshot val = case val of
  VMutable ref -> lift3 $ readIORef ref
  VObject name vals -> VObject name <$> mapM snapshot vals
  _ -> return val

lift3 = lift . lift . lift

log :: String -> Eval ()
log s = lift3 $ putStrLn s

log' :: [String] -> Eval ()
log' = concat ~> log

justV val = VObject "Just" [val]
nothingV = VObject "Nothing" []