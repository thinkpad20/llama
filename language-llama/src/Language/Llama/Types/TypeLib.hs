{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Llama.Types.TypeLib (
  Polytype(..), tTuple, TypeTable, TypeOf, TypeRecord,
  TypingState(..), Typing, Subs(..), NameSpace(..), Types(..),
  TypeEnv(..), Kind, pushNameSpace, popNameSpace, arrayOf, listOf,
  setOf, mapOf, numT, strT, unitT, maybeT, boolT, plain,
  fullName, (==>), (+:), nsTail, addToEnv, log, log',
  defaultTypingState, normalize, size, fromList, toList,
  vals, single, remove, charT, tConst) where

import qualified Data.Map as M
import qualified Prelude as P
import qualified Data.Set as S
import qualified Data.Text as T

import Language.Llama.Common.Common hiding (fromList, toList, delete, size)
import Language.Llama.Common.AST

type TypeTable = M.Map Name Type

-- | A polytype represents a type with zero or more type variables bound in
-- its scope. For example, Polytype ["a"] (TApply (TConst "Foo") TVar "a") is
-- a type @Foo a@, where a is free to be anything within that scope.
data Polytype = Polytype [Name] Type deriving (P.Show, Eq, Ord)
instance Render Polytype

type PurityEnv = M.Map Name PurityType
data PurityType = PTPure | PTLocal | PTImpure deriving (P.Show, Eq, Ord)
instance Monoid PurityType where
  mempty = PTPure
  a `mappend` b = case (a, b) of
    (PTPure, PTPure) -> PTPure
    (PTPure, PTLocal) -> PTLocal
    (PTLocal, PTPure) -> PTLocal
    (_, PTImpure) -> PTImpure
    (PTImpure, _) -> PTImpure

type Kind = Type

type TypeOf a = a -> Typing (Type, Subs)
type TypeRecord = (Kind, [(Name, Type)])
newtype NameSpace = NameSpace [Name] deriving P.Show
type Attribute = ()
data TypingState = TypingState { aliases :: M.Map Name Type
                               , nameSpace :: NameSpace
                               , typeEnv :: TypeEnv
                               , purityEnv :: PurityEnv
                               , knownTypes :: M.Map Name (Kind, [Attribute])
                               , freshName :: Name } deriving (P.Show)
type Typing = ErrorT ErrorList (StateT TypingState IO)

instance Monoid NameSpace where
  mempty = NameSpace mempty
  (NameSpace ns) `mappend` (NameSpace ns') = NameSpace (ns <> ns')

(+:) :: Name -> NameSpace -> NameSpace
name +: (NameSpace ns) = NameSpace (name : ns)

nsTail :: NameSpace -> NameSpace
nsTail (NameSpace ns) = NameSpace (tail ns)

defaultTypingState :: TypingState
defaultTypingState = TypingState { aliases = mempty
                                 , nameSpace = mempty
                                 , typeEnv = builtIns
                                 , purityEnv = builtInPurities
                                 , freshName = "a0"
                                 , knownTypes = mempty}

instance Render TypingState where
  render state =
    let (TE env) = typeEnv state
        bi = (\(TE b) -> b) builtIns
        env' = TE $ M.filterWithKey (\k _ -> M.notMember k bi) env in
    line $ mconcat ["Names: ", render env']

instance Render NameSpace where
  render (NameSpace ns) = T.intercalate "/" $ P.reverse ns

class Types t where
  -- Get the free type variables out of the type.
  free :: t -> S.Set Name
  -- Apply a set of type substitutions to the type.
  apply :: Subs -> t -> t

instance Types a => Types [a] where
  free list = mconcat $ map free list
  apply = map . apply

instance (Types a, Types b) => Types (a, b) where
  free (a, b) = free a <> free b
  apply subs (a, b) = (apply subs a, apply subs b)

instance (Ord a, Types a, Types b) => Types (M.Map a b) where
  free mp = free (M.keys mp) <> free (M.elems mp)
  apply subs mp = M.fromList (apply subs $ M.toList mp)

instance Types Type where
  free typ = case typ of
    TVar name -> S.singleton name
    TConst _ -> mempty
    TMod _ typ' -> free typ'
    TTuple types _ -> free types
    TApply t1 t2 -> free t1 <> free t2
    TFunction t1 t2 -> free t1 <> free t2
    TMultiFunc tset -> free tset

  apply subs@(Subs s) typ = case typ of
    TConst _ -> typ
    TMod mods typ' -> TMod mods $ apply subs typ'
    TVar name -> M.findWithDefault (TVar name) name s
    TTuple ts kw -> TTuple (apply subs ts) kw
    TApply t1 t2 -> TApply (apply subs t1) (apply subs t2)
    TFunction t1 t2 -> TFunction (apply subs t1) (apply subs t2)
    TMultiFunc tset -> TMultiFunc $ apply subs tset

instance Types Polytype where
  free (Polytype vars type_) = free type_ S.\\ (S.fromList vars)
  apply subs (Polytype vars type_) =
    let subs' = foldr delete subs vars in
    Polytype vars (apply subs' type_)

data Subs = Subs (M.Map Name Type)
          deriving (P.Show, Eq)

instance Render Subs where
  render (Subs s) = render s

instance Monoid Subs where
  mempty = Subs mempty
  mappend s@(Subs s1) (Subs s2) = Subs $ (M.map (apply s) s2) `M.union` s1

size :: Subs -> Int
size (Subs s) = M.size s

toList :: Subs -> [(Name, Type)]
toList (Subs s) = M.toList s

fromList :: [(Name, Type)] -> Subs
fromList = Subs . M.fromList

delete :: Name -> Subs -> Subs
delete n (Subs s) = Subs (M.delete n s)

vals :: Subs -> [Type]
vals (Subs s) = M.elems s

single :: Name -> Type -> Subs
single n t = Subs (M.singleton n t)

newtype TypeEnv = TE (M.Map Name Polytype) deriving (P.Show)

instance Render TypeEnv

instance Types TypeEnv where
  free (TE env) = free $ M.elems env
  apply subs (TE env) = TE $ apply subs <$> env

remove :: TypeEnv -> Name -> TypeEnv
remove (TE env) var = TE (M.delete var env)

addToEnv :: Name -> Polytype -> TypeEnv -> TypeEnv
addToEnv n p (TE env) = TE (M.insert n p env)

instance Monoid TypeEnv where
  mempty = TE mempty
  (TE a) `mappend` (TE b) = TE (a <> b)

instance Monoid Polytype where
  mempty = Polytype [] mempty
  Polytype vars t `mappend` Polytype vars' t' = Polytype (vars <> vars') (t <> t')

boolT, numT, strT, charT, unitT :: Type
arrayOf, listOf, setOf, maybeT :: Type -> Type
tTuple :: [Type] -> Type
tConst :: Name -> Type
mapOf :: (Type, Type) -> Type
(==>) :: Type -> Type -> Type

boolT = tConst "Bool"
numT = tConst "Num"
strT = tConst "Str"
charT = tConst "Char"
unitT = tTuple []
arrayOf = TApply (TConst "[]")
listOf = TApply (TConst "[!]")
setOf = TApply (TConst "{s}")
maybeT = TApply (TConst "Maybe")
tTuple ts = TTuple ts mempty
tConst = TConst
mapOf (key, val) = TApply (TApply (TConst "{}") key) val
(==>) = TFunction
infixr 4 ==>


builtIns :: TypeEnv
builtIns =
  TE $ M.fromList [ ("+", p nnn), ("-", p nnn), ("/", p nnn), ("<>", p sss)
                  , ("*", p nnn), ("%", p nnn), (">", p nnb), ("<", p nnb)
                  , (">=", p nnb), ("<=", p nnb), ("==", p nnb), ("!=", p nnb)
                  , ("<|", pab $ tup ab a b), ("|>", pab $ tup a ab b)
                  , ("~>", pabc $ tup ab bc ac), ("<~", pabc $ tup bc ab ac)
                  , ("println", pa $ a ==> unitT)
                  , ("show", p $ (numT ==> strT) <> (strT ==> strT))
                  , ("length", pa $ (strT ==> numT) <> (arrayOf a ==> numT))
                  , ("Just", pa $ a ==> maybeT a)
                  , ("Nothing", pa $ maybeT a)
                  , ("@call", p $ nnn <> sss <> vna)
                  , ("True", p boolT), ("False", p boolT)]
  where tup x y z = x ==> y ==> z
        nnn = tup numT numT numT
        sss = tup strT strT strT
        vna = tup (arrayOf a) numT a
        p = Polytype []
        pa = Polytype ["a"]
        pab = Polytype ["a", "b"]
        pabc = Polytype ["a", "b", "c"]
        nnb = tup numT numT boolT
        [a, b, c] = TVar <$> ["a", "b", "c"]
        (ab, bc, ac) = (a ==> b, b ==> c, a ==> c)

builtInPurities :: PurityEnv
builtInPurities = M.fromList [ ("+", PTPure), ("-", PTPure), ("/", PTPure), ("*", PTPure)
             , ("%", PTPure), (">", PTPure), ("<", PTPure), (">=", PTPure)
             , ("<=", PTPure), ("==", PTPure), ("!=", PTPure), ("<|", PTPure)
             , ("|>", PTPure), ("~>", PTPure), ("<~", PTPure), ("!_", PTPure)
             , ("_!", PTPure), ("print", PTImpure), ("show", PTPure)
             , ("length", PTPure), ("Just", PTPure), ("Nothing", PTPure)
             , ("@call", PTPure), ("True", PTPure), ("False", PTPure)
             , ("push!", PTLocal)
             ]

-- | Like generalizing, but creates a normal type. Basically it's just to
-- replace all the type variables with alphabet letters starting from "a".
normalize :: Type -> Type
normalize t = evalState (go t) ("a", mempty) where
  go type_ = case type_ of
    TVar name -> do
      (name', mapping) <- get
      case M.lookup name mapping of
        Nothing -> do modify (\(n, m) -> (next n, M.insert name name' m))
                      return (TVar name')
        Just n -> return (TVar n)
    TApply a b -> TApply <$> go a <*> go b
    TFunction a b -> TFunction <$> go a <*> go b
    TTuple ts _ -> tTuple <$> mapM go ts
    TConst _ -> return type_
    TMultiFunc set -> do
      let pairs = M.toList set
      pairs' <- forM pairs $ \(from, to) -> (,) <$> go from <*> go to
      return $ TMultiFunc (M.fromList pairs')
  next name = case T.last name of
    c | c < 'z' -> T.init name `T.snoc` succ c
      | True    -> name `T.snoc` 'a'

log :: T.Text -> Typing ()
log s = if hideLogs then return () else lift2 $ putStrLn $ T.unpack s

log' :: [T.Text] -> Typing ()
log' = mconcat ~> log

fullName :: Name -> Typing T.Text
fullName name = do
  (NameSpace ns) <- get <!> nameSpace
  return $ render (NameSpace $ name:ns)

pushNameSpace :: Name -> Typing ()
pushNameSpace name = do
  ns <- get <!> nameSpace
  modify $ \s -> s { nameSpace = name +: ns }

popNameSpace :: Typing ()
popNameSpace = do
  ns <- get <!> nameSpace
  modify $ \s -> s { nameSpace = nsTail ns }

plain :: Type -> Polytype
plain = Polytype []

hideLogs :: Bool
hideLogs = True
