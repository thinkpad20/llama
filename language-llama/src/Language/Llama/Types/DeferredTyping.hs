{-# LANGUAGE OverloadedStrings #-}
module Language.Llama.Types.DeferredTyping where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Prelude as P
import Language.Llama.Types.TypeLib
import Language.Llama.Common.Common hiding (member, insert)
import Language.Llama.Common.AST
import Language.Llama.Desugarer.Desugar (desugarIt)

data Assumption
  = Name :~ Type
  | Exists Name Type
  | HasTrait Name [Type]
  deriving (Eq, Ord, P.Show)

type TypeChecker = ErrorT ErrorList (State TypeCheckerState)
data TypeCheckerState = TCS
  { _tcsVarCount :: Int }
newtype Env = Env (M.Map Name Type, S.Set Assumption) deriving (P.Show)

instance Render Assumption where
  render (n :~ t) = n <> ": " <> render t
  render (Exists n t) = n <> " ~ " <> render t
  render (HasTrait n ts) = n <> " " <> T.intercalate " " (map render ts)

instance Render Env where
  render (Env (m, s)) = render s

makeVar :: TypeChecker Name
makeVar = do
  i <- _tcsVarCount <$> get
  let name = "a" <> render i
  modify $ \s -> s{_tcsVarCount = i+1}
  return name

newvar = TVar <$> makeVar

insert :: Assumption -> Env -> Env
insert a (Env (m, s)) = Env (m', S.insert a s) where
  m' = case a of {n :~ t -> M.insert n t m; _ -> m}

(!$) :: Env -> Name -> Type
Env (m, _) !$ name = m M.! name

member :: Name -> Env -> Bool
member n (Env (m, _)) = M.member n m

callable :: Type -> Type -> Type -> Assumption
callable a b c = HasTrait "Callable" [a, b, c]

instance Monoid Env where
  mempty = Env (mempty, mempty)
  Env (m, s) `mappend` Env (m', s') = Env (m <> m', s <> s')

defaultTCS :: TypeCheckerState
defaultTCS = TCS {_tcsVarCount=0}

typeof :: IsExpr e => (Env, e) -> TypeChecker (Env, Type)
typeof (env, expr) = case unExpr expr of
  Var n | member n env -> return (env, env !$ n)
  Var n -> do
    var <- newvar
    return (insert (n :~ var) env, var)
  Define n e -> do
    (env', t) <- typeof (env, e)
    return (env <> insert (n :~ t) env', t)
  Then e1 e2 -> do
    (env1, _) <- typeof (env, e1)
    (env2, t) <- typeof (env <> env1, e2)
    return (env <> env1 <> env2, t)
  Lambda x e -> do
    var <- newvar
    (env', t) <- typeof (insert (x :~ var) env, e)
    return (env <> env', var ==> t)
  Apply e1 e2 -> do
    (env1, t1) <- typeof (env, e1)
    (env2, t2) <- typeof (env <> env1, e2)
    var <- newvar
    let env' = env <> env1 <> env2
    return (insert (callable t1 t2 var) env', var)

runTyping :: IsExpr e => e -> Either ErrorList (Env, Type)
runTyping e = evalState (runErrorT $ typeof (mempty, e)) defaultTCS

typeIt :: String -> Either ErrorList (Env, Type)
typeIt input = case desugarIt input of
  Left err -> Left $ ErrorList [show err]
  Right expr -> runTyping expr

testString :: String -> IO ()
testString input = case typeIt input of
  Left err -> error $ P.show err
  Right (env, _type) -> do
    let s = render env <> " " <> render _type
    putStrLn $ unpack $ s

-- | Returns if the two types can be unified.
cbu :: Type -> Type -> TypeChecker Bool
cbu t1 t2 = (unify t1 t2 >> return True) `catchError` \_ -> return False

-- | Binds a name to a type, as long as it's not an infinite type.
bind :: Name -> Type -> TypeChecker Subs
bind name typ = case typ of
  TVar n | n == name -> return mempty
  _ -> if name `S.member` free typ then occursCheck
       else return (Subs $ M.singleton name typ)
  where occursCheck = throwErrorC ["Occurs check"]

-- findCbu :: Env -> Name ->

unify :: Type -> Type -> TypeChecker Subs
unify t1 t2 = case (t1, t2) of
  (TVar name, typ) -> bind name typ
  (typ, TVar name) -> bind name typ
  (TConst n, TConst n') | n == n' -> return mempty
  (TApply a b, TApply a' b') -> do
    subs1 <- unify a a'
    subs2 <- unify (apply subs1 b) (apply subs1 b')
    return (subs1 <> subs2)
  (type1, type2) -> throwErrorC $ [ "Incompatible types: `", render type1
                                  , "' and `", render type2, "'"]
