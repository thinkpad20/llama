{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module EvalLib where

import Prelude (IO, Show(..), Eq(..), Ord(..), Bool(..)
               , Double, Maybe(..), undefined, Monad(..)
               , ($), Int, (.), (*), Either(..), String
               , fst, (+), (-), (/), (=<<), otherwise, mapM_)
import qualified Prelude as P
import qualified Data.HashTable.IO as H
import Data.HashMap hiding (lookup, (!))
import "hashmap" Data.HashSet
import qualified "hashmap" Data.HashSet as S
import Data.Text
import qualified Data.Vector.Persistent as V
import qualified Data.Array.IO as A

import Common hiding (intercalate)
import AST
import Desugar

type Array a = A.IOArray a

type HashTable k v = H.BasicHashTable k v
type Env = HashTable Name Value
type PMap = Map Value Value
type MMap = HashTable Value Value
type PSet = Set Value
type MSet = HashTable Value Bool

data Value = VNumber !Double
           | VString !Text
           | VVector !(V.Vector Value)
           | VTuple  !(V.Vector Value)
           | VException !Text
           | PMap !PMap
           | MMap !MMap
           | PSet !PSet
           | MSet !MSet
           | VLocal !LocalRef
           | Closure !Expr !Env
           | VObj !Obj
           | Builtin !Builtin
           deriving (Show)

instance Render Value

type Builtin = (Name, Value -> Eval Value)

instance Show Builtin where
  show (name, _) = "BUILTIN: " <> unpack name
instance Render Builtin where
  render (name, _) = "BUILTIN: " <> name

data LocalRef = Arg
              | ArgRef Int LocalRef
              deriving (Show)

instance Render LocalRef

data Obj = Obj {
    outerType :: !Type
  , attribs   :: !Env
  }
  deriving (Show)

newtype EvalState = ES [Frame]
data Frame = Frame {
    eEnv :: Env
  , eArg :: Value
  }

type Eval = ErrorT ErrorList (StateT EvalState IO)

new :: Eval Env
new = lift2 H.new

pushFrame :: Value -> Env -> Eval ()
pushFrame arg env = do
  let frame = Frame {eArg=arg, eEnv=env}
  modify $ \(ES frames) -> ES (frame:frames)

popFrame :: Eval ()
popFrame = get >>= \case
  ES [] -> throwError1 "Tried to pop an empty stack"
  ES (_:frames) -> put $ ES frames

hInsert :: Env -> Name -> Value -> Eval ()
hInsert env key val = lift2 $ H.insert env key val

hLookup :: Env -> Name -> Eval (Maybe Value)
hLookup env key = lift2 $ H.lookup env key

renderNS :: [Name] -> Name
renderNS names = P.reverse names ! intercalate "/"

lookup :: Name -> Eval (Maybe Value)
lookup name = do
  ES stack <- get
  lift2 $ loop stack
  where loop :: [Frame] -> IO (Maybe Value)
        loop [] = return Nothing
        loop (env:rest) = H.lookup (eEnv env) name >>= \case
          Nothing -> loop rest
          val -> return val


lookupOrError :: Name -> Eval Value
lookupOrError name = lookup name >>= \case
  Just val -> return val
  Nothing -> throwErrorC ["Name '", name, "' is not defined"]

eval :: Expr -> Eval Value
eval = \case
  Number n -> return (VNumber n)
  String s -> return (VString s)
  Constructor n -> lookupOrError n
  Var n -> lookupOrError n
  Block exprs -> evalBlock exprs
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
  Attribute expr name -> eval expr >>= getAttribute name
  Lambda param body -> evalLamda param body
  Define name expr -> store name =<< eval expr
  expr -> throwErrorC ["Can't handle ", render expr]

store :: Name -> Value -> Eval Value
store name val = do
  ES (f:_) <- get
  hInsert (eEnv f) name val
  pure val

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

renderEnv :: Env -> Eval Text
renderEnv env = lift2 $ do
  pairs <- H.toList env
  let pairs' = P.map (\(k, v) -> k <> ": " <> render v) pairs
  return $ intercalate ", " pairs'

getAttribute :: Name -> Value -> Eval Value
getAttribute = undefined

getClosure :: Expr -> Expr -> Eval Env
getClosure (Var argName) expr = do
  newEnv <- new
  runStateT (go newEnv expr) (S.singleton argName)
  return newEnv
  where
    go :: Env -> Expr -> StateT (Set Name) Eval ()
    go env = \case
      Number _ -> return ()
      String _ -> return ()
      Block es -> mapM_ (go env) es
      Constructor name -> lift (lookupOrError name) >>= lift . hInsert env name
      Var name | name == argName -> lift $ hInsert env name (VLocal Arg)
               | otherwise -> S.member name <$> get >>= \case
                 True -> return ()
                 False -> lift . hInsert env name =<< lift (lookupOrError name)
      Apply a b -> go env a >> go env b
      Lambda param body -> getNames param >> go env body
      Define var ex -> modify (S.insert var) >> go env ex
      e -> lift $ throwErrorC ["Can't get closure of ", render e]
    getNames :: Expr -> StateT (Set Name) Eval ()
    getNames = \case
      Var name -> modify $ S.insert name
      Apply a b -> getNames a >> getNames b
      Tuple es kws -> do
        mapM_ getNames es
        let kwNames = S.fromList (P.map fst kws)
        modify $ S.union kwNames
      Number _ -> return ()
      String _ -> return ()
      -- For constructors, we should probably look in surrounding env
      -- first...?
      Constructor _ -> return ()
      param -> lift $ throwErrorC ["Invalid function parameter: ", render param]

builtIns :: IO Env
builtIns = H.fromList
  [
    ("println", Builtin bi_println)
  , ("+", Builtin bi_plus)
  , ("-", Builtin bi_minus)
  , ("*", Builtin bi_times)
  , ("/", Builtin bi_divide)
  , ("negate", Builtin bi_negate)
  ]

bi_println :: Builtin
bi_println = ("println", println) where
  println val = lift2 (p val) >> pure unitV
  p (VNumber n) = P.print n
  p (VString s) = P.putStrLn $ unpack s
  p val = P.print val

bi_negate :: Builtin
bi_negate = ("negate", neg) where
  neg (VNumber n) = pure $ VNumber $ P.negate n
  neg val = numTypeError val

bi_plus, bi_minus, bi_divide, bi_times :: Builtin
bi_plus = bi_binary "+" (+)
bi_minus = bi_binary "-" (-)
bi_times = bi_binary "*" (*)
bi_divide = bi_binary "/" (/)

bi_binary :: Name -> (Double -> Double -> Double) -> Builtin
bi_binary name op = (name, f) where
  f (VNumber n) = pure $ Builtin (render n <> name, fN n)
  f (VLocal ref) = deref ref >>= \case
    VNumber n -> pure $ Builtin (render n <> name, fN n)
    val -> numTypeError val
  f val = numTypeError val
  fN n (VNumber n') = pure $ VNumber (op n n')
  fN n (VLocal ref) = fN n =<< deref ref
  fN _ val = numTypeError val

deref :: LocalRef -> Eval Value
deref Arg = do
  log' ["Dereferencing arg"]
  ES (f:_) <- get
  pure (eArg f)
deref ref = error $ "Can't handle reference type " <> render ref

unitV :: Value
unitV = VTuple mempty

numTypeError :: Value -> Eval a
numTypeError val =
  throwErrorC ["Value `", render val, "' is not of type Num"]

error :: Text -> a
error msg = P.error $ unpack msg

runEval :: Expr -> IO (Either ErrorList Value, EvalState)
runEval expr = do
  env <- builtIns
  let frame = Frame {eEnv=env, eArg=unitV}
      initState = ES [frame]
  runStateT (runErrorT (eval expr)) initState

evalIt :: String -> IO (Either ErrorList Value)
evalIt input = case desugarIt input of
  Left err -> return (Left $ ErrorList [render err])
  Right expr -> fst <$> runEval expr

log :: Text -> Eval ()
log s = if hideLogs then return () else lift2 $ P.putStrLn $ unpack s

log' :: [Text] -> Eval ()
log' = mconcat ~> log

hideLogs :: Bool
hideLogs = False
