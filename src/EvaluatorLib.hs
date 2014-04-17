{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module EvaluatorLib where

import Prelude (IO, Eq(..), Ord(..), Bool(..)
               , Double, Maybe(..), undefined, Monad(..)
               , ($), Int, (.), (+), Either(..), String
               , fst, (=<<), otherwise, fmap)
import qualified Prelude as P
import qualified Data.HashTable.IO as H
import Data.HashMap hiding (lookup, (!), toList)
import "hashmap" Data.HashSet hiding (toList)
import qualified "hashmap" Data.HashSet as S
import Data.Text hiding (length)
import Data.Sequence
import qualified Data.Array.IO as A
import Data.IORef
import qualified Data.Foldable as F

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
           | VReturn !Value
           | VBreak  !Value
           | VThrow  !Value
           | VMaybe !(Maybe Value)
           | VContinue
           | PMap !PMap
           | MMap !MMap
           | PSet !PSet
           | MSet !MSet
           | VRef !(IORef Value)
           | VLocal !LocalRef
           | Closure !(Maybe Name) !Expr !Env
           | VInstance !Instance
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
    VReturn (VTuple tup) | length tup == 0 -> "return"
    VReturn val -> "return " <> render val
    VBreak (VTuple tup) | length tup == 0 -> "break"
    VBreak val -> "break " <> render val
    VThrow val -> "throw " <> render val
    VContinue -> "continue"
    VInstance inst -> show inst
    Closure (Just name) _ _ -> "Function `" <> name <> "'"
    Closure Nothing expr _ -> "Anonymous Function {" <> render expr <> "}"
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

data Instance = Instance {
    instType   :: !Type
  , instConstr :: !Text
  , instParent :: !(Maybe Value)
  , instValues :: !(Seq Value)
  , instAttrs  :: !(Maybe Env)
  }
  deriving (P.Show)

data EvalState = ES {
    esStack :: [Frame]
  , esInstrCount :: IORef Int
  }
data Frame = Frame {
    eEnv :: !Env
  , eTrace :: !StackTraceEntry
  , eArg :: !Value
  }

data StackTraceEntry = STE {
    steLine :: !Int
  , steColumn :: !Int
  , steFuncName :: !Text
  }

defSTE :: StackTraceEntry
defSTE = STE {steLine=0, steColumn=0, steFuncName=""}

addCount :: Eval ()
addCount = do
 !ES {esInstrCount=c} <- get
 lift2 $ modifyIORef c (+1)

type Eval = ErrorT ErrorList (StateT EvalState IO)

writeRef :: IORef Value -> Value -> Eval Value
writeRef !ref !val = lift2 (writeIORef ref val) >> pure val

readRef :: IORef Value -> Eval Value
readRef !ref = lift2 (readIORef ref)

nothingV :: Value
nothingV = VMaybe Nothing

justV :: Value -> Value
justV val = VMaybe (Just val)

new :: Eval Env
new = lift2 H.new

pushFrame :: Name -> Value -> Env -> Eval ()
pushFrame !name !arg !env = do
  let frame = Frame {eArg=arg, eEnv=env, eTrace=trace}
  modify $ \es@(ES {esStack=frames}) -> es {esStack=frame:frames}
  where trace = defSTE {steFuncName=name}

popFrame :: Eval ()
popFrame = get >>= \case
  ES {esStack=[]} -> throwError1 "Tried to pop an empty stack"
  es@(ES {esStack=(_:frames)}) -> put es {esStack=frames}

hInsert :: Env -> Name -> Value -> Eval ()
hInsert !env !key !val = lift2 $ H.insert env key val

hLookup :: Env -> Name -> Eval (Maybe Value)
hLookup !env !key = lift2 $ H.lookup env key

renderNS :: [Name] -> Name
renderNS !names = P.reverse names ! intercalate "/"

lookup :: Name -> Eval (Maybe Value)
lookup !name = do
  !ES {esStack=stack} <- get
  lift2 $ loop stack
  where loop :: [Frame] -> IO (Maybe Value)
        loop ![] = return Nothing
        loop !(env:rest) = H.lookup (eEnv env) name >>= \case
          Nothing -> loop rest
          val -> return val

newRef :: Value -> Eval Value
newRef !val = lift2 $ VRef <$> newIORef val

unbox :: Value -> Eval Value
unbox !(VRef ref) = lift2 $ readIORef ref
unbox !val = return val

lookupOrError :: Name -> Eval Value
lookupOrError !name = lookup name >>= \case
  Just val -> return val
  Nothing -> throwErrorC ["Name '", name, "' is not defined"]

store :: Name -> Value -> Eval Value
store !name !val = do
  ES {esStack=(f:_)} <- get
  hInsert (eEnv f) name val
  pure val

renderEnv :: Env -> Eval Text
renderEnv !env = lift2 $ do
  pairs <- H.toList env
  let pairs' = P.map (\(k, v) -> k <> ": " <> render v) pairs
  return $ intercalate ", " pairs'

getAttribute :: Name -> Value -> Eval Value
getAttribute = undefined

typeError :: Name -> Value -> Eval a
typeError !expect !val = throwErrorC ["Expected a value of type `", expect
                                   , "', but got a `", render val, "'"]

getClosure :: Expr -> Expr -> Eval Env
getClosure !pattern !expr = case pattern of
  Var argName -> do
    newEnv <- new
    runStateT (loop argName newEnv expr) [S.singleton argName]
    return newEnv
  _ -> throwErrorC ["Can't handle pattern `", render pattern, "'"]
  where
    loop :: Name -> Env -> Expr -> StateT [Set Name] Eval ()
    loop !argName !env = \case
      Number _ -> return ()
      String _ -> return ()
      Block es -> mapM_ go es
      Tuple es kws | kws == mempty -> mapM_ go es
                   | otherwise -> lift $ throwErrorC ["Kwarg closures aren't implemented"]
      Constructor name -> lift (lookupOrError name) >>= lift . hInsert env name
      Var name | name == argName -> lift $ hInsert env name (VLocal Arg)
               | otherwise -> findName name >>= \case
                 True -> return ()
                 False -> lift . hInsert env name =<< lift (lookupOrError name)
      Apply a b -> go a >> go b
      Lambda param body -> push >> getNames param >> go body <* pop
      Define var ex -> addName var >> go ex
      Assign e e' -> go e >> go e'
      If c t f -> go c >> go t >> go f
      Literal (ArrayLiteral exprs) -> mapM_ go exprs
      TryCatch toTry ifErr finally -> go toTry >> mapM_ goPat ifErr >> goMayb finally
      Throw e -> go e
      Return e -> go e
      Modified _ e -> go e
      For e1 e2 e3 e4 -> go e1 >> go e2 >> go e3 >> go e4
      e -> lift $ throwErrorC ["Can't get closure of ", render e]
      where go = loop argName env
            goPat (_, res) = go res
            goMayb = \case {Nothing -> return (); Just e -> go e}
    getNames :: Expr -> StateT [Set Name] Eval ()
    getNames !ex = case ex of
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
deref !Arg = do
  log' ["Dereferencing arg"]
  ES {esStack=(f:_)} <- get
  pure (eArg f)
deref !ref = error $ "Can't handle reference type " <> render ref

-- A more powerful version of (==), because it operates in the IO monad.
isEqual :: Value -> Value -> Eval Bool
isEqual v1 v2 = case (v1, v2) of
  (VNumber n, VNumber n') -> return $ n == n'
  (VBool b, VBool b') -> return $ b == b'
  (VString s, VString s') -> return $ s == s'
  (VVector v, VVector v') -> do
    let zipped = P.zip (F.toList v) (F.toList v')
    P.all (== True) <$> P.mapM (P.uncurry isEqual) zipped
  (VTuple t, VTuple t') -> do
    let zipped = P.zip (F.toList t) (F.toList t')
    P.all (== True) <$> P.mapM (P.uncurry isEqual) zipped
  (VReturn v, VReturn v') -> isEqual v v'
  (VBreak v, VBreak v') -> isEqual v v'
  (VThrow v, VThrow v') -> isEqual v v'
  (VMaybe (Just v), VMaybe (Just v')) -> isEqual v v'
  (VMaybe (Just v), VMaybe Nothing) -> return False
  (VMaybe Nothing, VMaybe (Just v)) -> return False
  (VMaybe Nothing, VMaybe Nothing) -> return True
  (VContinue, VContinue) -> return True
  (VRef r, VRef r') -> do
    (v, v') <- (,) <$> readRef r <*> readRef r'
    isEqual v v'
  (_, _) -> throwErrorC [ "Can't determine if vals ", render v1
                        , " and ", render v2, " are equal"]
           -- | PMap !PMap
           -- | MMap !MMap
           -- | PSet !PSet
           -- | MSet !MSet
           -- | VLocal !LocalRef
           -- | Closure !(Maybe Name) !Expr !Env
           -- | VInstance !Instance
           -- | Builtin !Builtin

indexOf :: Int -> Instance -> Eval Value
indexOf idx (instValues -> vals)
  | idx < length vals = return $ Data.Sequence.index vals idx
  | otherwise = throwErrorC ["Index out of range on object"]

unitV :: Value
unitV = VTuple mempty

numTypeError :: Value -> Eval a
numTypeError !val =
  throwErrorC ["Value `", render val, "' is not of type Num"]

error :: Text -> a
error !msg = P.error $ unpack msg

print :: Render a => a -> IO ()
print !x = renderIO x >>= putStrLn

putStrLn :: Text -> IO ()
putStrLn !t = P.putStrLn $ unpack t

log :: Text -> Eval ()
log !s = if hideLogs then return () else lift2 $ P.putStrLn $ unpack s

log' :: [Text] -> Eval ()
log' = mconcat ~> log

doTypeChecking :: Bool
doTypeChecking = False

hideLogs :: Bool
hideLogs = True
