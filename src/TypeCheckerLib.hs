{-# LANGUAGE OverloadedStrings #-}
module TypeCheckerLib where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Common

-- | A variable can be rigid (fixed in scope), or polymorphic (free to take on
-- multiple forms in the same scope).
data Type = TRigidVar  !Name
          | TVar       !Name
          | TConst     !Name
          | TTuple     ![Type]
          | TApply     !Type !Type
          | TFunction  !Type !Type
          | TMut       !Type
          | TMod       !Mod !Type
          | TMultiFunc !(M.Map Type Type)
          deriving (Show, Eq, Ord)

data Mod = Mut | Ref | Pure | Local deriving (Show, Eq, Ord)

-- | A polytype represents a type with zero or more type variables bound in
-- its scope. For example, Polytype ["a"] (TApply (TConst "Foo") TVar "a") is
-- a type @Foo a@, where a is free to be anything within that scope.
data Polytype = Polytype [Name] Type

newtype Subs    = Subs (M.Map Name Type)
newtype TypeEnv = TE (M.Map Name Type)

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

instance Types TypeEnv where
  free (TE env) = free $ M.elems env
  apply subs (TE env) = TE $ apply subs <$> env

remove :: TypeEnv -> Name -> TypeEnv
remove (TE env) var = TE (M.delete var env)

instance Types Type where
  free typ = case typ of
    TVar name -> S.singleton name
    TConst _ -> mempty
    TMod _ typ' -> free typ'
    TTuple types -> free types
    TApply t1 t2 -> free t1 <> free t2
    TFunction t1 t2 -> free t1 <> free t2
    TMultiFunc tset -> free tset

  apply subs@(Subs s) typ = case typ of
    TConst _ -> typ
    TMod mods typ' -> TMod mods $ apply subs typ'
    TVar name -> M.findWithDefault (TVar name) name s
    TTuple ts -> TTuple $ apply subs ts
    TApply t1 t2 -> TApply (apply subs t1) (apply subs t2)
    TFunction t1 t2 -> TFunction (apply subs t1) (apply subs t2)
    TMultiFunc tset -> TMultiFunc $ apply subs tset

instance Monoid Subs where
  mempty = Subs mempty
  mappend s@(Subs s1) (Subs s2) = Subs $ (M.map (apply s) s2) `M.union` s1

instance Render Type where
  render t = case t of
    TRigidVar name -> name
    TVar name -> {-"*" <> -} name {-<> "*"-}
    TConst name -> name
    TTuple ts -> "(" <> T.intercalate ", " (map render ts) <> ")"
    TApply (TConst "[]") typ -> "[" <> render typ <> "]"
    TApply (TConst "[!]") typ -> "[!" <> render typ <> "]"
    TApply a b -> render a <> " " <> render' b
    TFunction t1 t2 -> render'' t1 <> " -> " <> render t2
    TMut typ -> "mut " <> render typ
    TMultiFunc tset -> "{" <> renderSet tset <> "}"
    where render' typ = case typ of
            TApply _ _ -> "(" <> render typ <> ")"
            _ -> render typ
          render'' typ = case typ of
            TFunction _ _ -> "(" <> render typ <> ")"
            _ -> render typ
          renderSet tset =
            let pairs = M.toList tset
                rPair (from, to) = render from <> " -> " <> render to
            in T.intercalate ", " (map rPair pairs)

instance Monoid Type where
  mempty = TMultiFunc mempty
  TMultiFunc s `mappend` TMultiFunc s' = TMultiFunc $ M.union s s'
  TMultiFunc s `mappend` TFunction from to = TMultiFunc $ M.insert from to s
  TFunction from to `mappend` TMultiFunc s = TMultiFunc $ M.insert from to s
  TFunction f1 t1 `mappend` TFunction f2 t2 =
    TMultiFunc $ M.fromList [(f1, t1), (f2, t2)]
  t1 `mappend` t2 = error $ "Invalid `or`s: " <> show t1 <> ", " <> show t2

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
tTuple = TTuple
tConst = TConst
mapOf (key, val) = TApply (TApply (TConst "{}") key) val
(==>) = TFunction
infixr 4 ==>
