{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module EvaluatorLib where

import Prelude (IO, Eq(..), Ord(..), Bool(..)
               , Double, Maybe(..), undefined, Monad(..)
               , ($), Int, (.), (*), Either(..), String
               , fst, (+), (-), (/), (=<<), otherwise, fmap)
import qualified Prelude as P
import qualified Data.HashTable.IO as H
import Data.HashMap hiding (lookup, (!), toList)
import "hashmap" Data.HashSet hiding (toList)
import qualified "hashmap" Data.HashSet as S
import Data.Text
import Data.Sequence
import qualified Data.Array.IO as A
import Data.IORef

import Common hiding (intercalate)
import AST
import TypeLib hiding (log, log', vals, toList)

type Array a = A.IOArray a

type HashTable k v = H.BasicHashTable k v
type Env = HashTable Name Value
type PMap = Map Value Value
type MMap = HashTable Value Value
type PSet = Set Value
type MSet = HashTable Value Bool

data Value = VNumber !Double
           | VBool   !Bool
           | VString !Text
           | VVector !(Seq Value)
           | VTuple  !(Seq Value)
           | PMap !PMap
           | MMap !MMap
           | PSet !PSet
           | MSet !MSet
           | VRef !(IORef Value)
           | VLocal !LocalRef
           | Closure !Expr !Env
           | VObj !Obj
           | Builtin !Builtin
           deriving (P.Show)

type Builtin = (Name, Value -> Eval Value)

instance P.Show (IORef Value) where
  show _ = "(reference)"
instance Render Value where
  render = \case
    VNumber n | isInt n -> show (P.floor n :: Int)
    VNumber n -> show n
    VBool b -> show b
    VString s -> show s
    VVector vals -> "[" <> intercalate "," (toList $ fmap render vals) <> "]"
    VTuple  vals -> "(" <> intercalate "," (toList $ fmap render vals) <> ")"
    VLocal _ -> "(some reference)"
    VObj obj -> show obj
    Closure expr _ -> "Closure(" <> render expr <> ")"
    Builtin (name, _) -> "Builtin: " <> name
    val -> show val
  renderIO (VRef ref) = render <$> readIORef ref
  renderIO val = return $ render val

instance P.Show Builtin where
  show (name, _) = "BUILTIN: " <> unpack name
instance Render Builtin where
  render (name, _) = "BUILTIN: " <> name

data LocalRef = Arg
              | ArgRef Int LocalRef
              deriving (P.Show)

instance Render LocalRef

data Obj = Obj {
    outerType :: !Type
  , innerType :: Text
  , attribs   :: !(Maybe Env)
  }
  deriving (P.Show)

_obj :: Obj
_obj = Obj {outerType=unitT, innerType="", attribs=Nothing}

newtype EvalState = ES [Frame]
data Frame = Frame {
    eEnv :: Env
  , eArg :: Value
  }

type Eval = ErrorT ErrorList (StateT EvalState IO)

writeRef :: IORef Value -> Value -> Eval Value
writeRef ref val = lift2 (writeIORef ref val) >> pure val

readRef :: IORef Value -> Eval Value
readRef ref = lift2 (readIORef ref)

nothingV :: Value
nothingV =
  VObj $ _obj {
    outerType = TConst "Maybe"
  , innerType = "Nothing"
  }

justV :: Value -> Eval Value
justV val = do
  attrs <- new
  hInsert attrs "0" val
  return $ VObj $ _obj {
    outerType = TConst "Maybe"
  , innerType = "Just"
  , attribs = Just attrs
  }

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

newRef :: Value -> Eval Value
newRef val = lift2 $ VRef <$> newIORef val

unbox :: Value -> Eval Value
unbox (VRef ref) = lift2 $ readIORef ref
unbox val = return val

unboxTo :: (Value -> Eval Value) -> Value -> Eval Value
unboxTo func val = unbox val >>= func

lookupOrError :: Name -> Eval Value
lookupOrError name = lookup name >>= \case
  Just val -> return val
  Nothing -> throwErrorC ["Name '", name, "' is not defined"]

store :: Name -> Value -> Eval Value
store name val = do
  ES (f:_) <- get
  hInsert (eEnv f) name val
  pure val

renderEnv :: Env -> Eval Text
renderEnv env = lift2 $ do
  pairs <- H.toList env
  let pairs' = P.map (\(k, v) -> k <> ": " <> render v) pairs
  return $ intercalate ", " pairs'

getAttribute :: Name -> Value -> Eval Value
getAttribute = undefined

typeError :: Name -> Value -> Eval a
typeError expect val = throwErrorC ["Expected a value of type `", expect
                                   , "', but got a `", render val, "'"]

getClosure :: Expr -> Expr -> Eval Env
getClosure (Var argName) expr = do
  newEnv <- new
  runStateT (go newEnv expr) [S.singleton argName]
  return newEnv
  where
    go :: Env -> Expr -> StateT [Set Name] Eval ()
    go env = \case
      Number _ -> return ()
      String _ -> return ()
      Block es -> mapM_ (go env) es
      Tuple es kws | kws == mempty -> mapM_ (go env) es
                   | otherwise -> lift $ throwErrorC ["Kwarg closures aren't implemented"]
      Constructor name -> lift (lookupOrError name) >>= lift . hInsert env name
      Var name | name == argName -> lift $ hInsert env name (VLocal Arg)
               | otherwise -> findName name >>= \case
                 True -> return ()
                 False -> lift . hInsert env name =<< lift (lookupOrError name)
      Apply a b -> go env a >> go env b
      Lambda param body -> push >> getNames param >> go env body <* pop
      Define var ex -> addName var >> go env ex
      If c t f -> go env c >> go env t >> go env f
      Literal (ArrayLiteral exprs) -> mapM_ (go env) exprs
      e -> lift $ throwErrorC ["Can't get closure of ", render e]
    getNames :: Expr -> StateT [Set Name] Eval ()
    getNames = \case
      Var name -> addName name
      Apply a b -> getNames a >> getNames b
      Tuple es kws -> do
        mapM_ getNames es
        forM_ (P.map fst kws) addName
      Number _ -> return ()
      String _ -> return ()
      -- For constructors, we should probably look in surrounding env
      -- first...?
      Constructor _ -> return ()
      param -> lift $ throwErrorC ["Invalid function parameter: ", render param]
    addName :: Name -> StateT [Set Name] Eval ()
    addName name = do
      s:ss <- get
      put $ (S.insert name s):ss
    push = modify $ \s -> mempty:s
    pop = modify $ \(_:ss) -> ss
    findName :: Name -> StateT [Set Name] Eval Bool
    findName name = get >>= look where
      look = \case
        [] -> return False
        (s:ss) -> if S.member name s then return True else look ss

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

print :: Render a => a -> IO ()
print x = renderIO x >>= putStrLn

putStrLn :: Text -> IO ()
putStrLn t = P.putStrLn $ unpack t

log :: Text -> Eval ()
log s = if hideLogs then return () else lift2 $ P.putStrLn $ unpack s

log' :: [Text] -> Eval ()
log' = mconcat ~> log

doTypeChecking :: Bool
doTypeChecking = False

hideLogs :: Bool
hideLogs = True
