{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Language.Llama.Types.TypeCheckLib (
    module Language.Llama.Common.Common
  , module Language.Llama.Common.AST
  , module Language.Llama.Parser.Parser
  , module Language.Llama.Desugarer.Desugar
  , Instance(..), DefaultEntry(..), DefaultInstance(..), TypeMap, TraitMap
  , Substitutable(..), Substitution(..), TCState(..), TypeChecker
  , (\\), (•), oneTrait, oneTrait', compose, oneSub
  ) where

import qualified Prelude as P
import Prelude (Show)
import Data.Set (Set, singleton, (\\))
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Language.Llama.Common.Common hiding (singleton)
import Language.Llama.Common.AST
import Language.Llama.Parser.Parser (Sourced(..))
import Language.Llama.Desugarer.Desugar hiding (test)
import System.IO.Unsafe (unsafePerformIO)

-- | Stores names that we've typed.
type TypeMap = Map Name Polytype
-- | The working stack in our type checker state.
type TCStack = [TCStackFrame]
data TCStackFrame = TCStackFrame {_tyMap :: TypeMap, _trMap :: TraitMap}
data TCState' = TCState' {_count'::Int, _stack::TCStack, _aexpr::AnnExpr}
-- | Our monadic state. The count is for generating new names.
data TCState = TCState {_count::Int, _tymaps::[TypeMap], _trmaps::[TraitMap]}
-- | The main type checking monad. Ye olde errorT stateT.
type TypeChecker = ErrorT ErrorList (StateT TCState IO)
-- | An expression annotated with its type.
data AnnExpr = AnnExpr { _expr :: AbsExpr AnnExpr, _type :: Type }

--------------------------------------------------------------------------
---------------------------- Trait instances -----------------------------
--------------------------------------------------------------------------

-- | A stored trait instance, which might have its own context.
data Instance = Instance Context [BaseType] deriving (Show)

instance Render Instance where
  render (Instance ctx ts) = render ctx <> ". " <> T.intercalate " " ts' where
    ts' = fmap renderP ts

-- | An entry in a default instance, which is either some fixed type, or a
-- "slot," which is a type into which an (otherwise ambiguous) type variable 
-- can be converted.
data DefaultEntry = Slot BaseType | Fixed BaseType deriving (Show)

instance Render DefaultEntry where
  render (Slot t) = "<" <> render t <> ">"
  render (Fixed t) = render t

-- | A list of DefaultEntries makes a single DefaultInstance.
newtype DefaultInstance = DefaultInstance [DefaultEntry] deriving (Show)

instance Render DefaultInstance where
  render (DefaultInstance ts) = "default " <> ts' where
    ts' = T.intercalate " " $ fmap render ts

-- | Stores the instances we've made.
type TraitMap = Map Name ([Instance], [DefaultInstance])

--------------------------------------------------------------------------
-------------------------- Type substitutions ----------------------------
--------------------------------------------------------------------------

-- | A mapping from (type variable) names to BaseTypes.
newtype Substitution = Substitution (Map Name BaseType) deriving (Show, Eq)

instance Render Substitution where
  render (Substitution s) = "{" <> T.intercalate ", " items <> "}" where
    items = fmap (\(f,t) -> render f <> "=>" <> render t) $ H.toList s

-- | Composes two substitutions. The newer substitution should go second.
(•) :: Substitution -> Substitution -> Substitution
Substitution s1 • Substitution s2 = Substitution (s1' <> s2) where
  s1' = fmap (Substitution s2 •>) s1

-- | Composes a list of substitutions.
compose :: [Substitution] -> Substitution
compose = foldl' (•) zero

-- | A singleton substitution.
oneSub :: Name -> BaseType -> Substitution
oneSub n = Substitution . H.singleton n

-- | Removes a name from a substitution.
remove :: Name -> Substitution -> Substitution
remove n (Substitution s) = Substitution (delete n s)

-- | Substitutions can be empty, but aren't monoids.
instance Zero Substitution where zero = Substitution mempty

-- | Basically anything that has type variables can have substitutions applied.
class Substitutable a where (•>) :: Substitution -> a -> a
instance Substitutable BaseType where
  subs@(Substitution s) •> t = case t of
    TVar n | member n s -> s H.! n
    TApply t1 t2 -> TApply (subs •> t1) (subs •> t2)
    _ -> t
instance Substitutable Assertion where
  subs •> (HasTrait n ts) = HasTrait n $ fmap (subs •>) ts
instance Substitutable Context where
  subs •> (Context ctx) = Context $ S.map (subs •>) ctx
instance Substitutable Type where
  subs •> (Type ctx t) = Type (subs •> ctx) (subs •> t)
instance Substitutable Polytype where
  subs •> (Polytype vars t) = (Polytype vars (s t)) where
    s = (S.foldr' remove subs vars •>)
instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  subs •> (a, b) = (subs •> a, subs •> b)
instance (Substitutable val) => Substitutable (Map key val) where
  subs •> m = fmap (subs •>) m
instance Substitutable a => Substitutable [a] where
  subs •> list = fmap (subs •>) list
instance Substitutable AnnExpr where
  subs •> AnnExpr e t = AnnExpr (fmap (subs •>) e) (subs •> t)

instance FreeVars TypeMap where freevars = freevars . H.elems

-- | A singleton context with a trait using only type variables.
oneTrait' :: Name -> [Name] -> Context
oneTrait' name = oneTrait name . fmap TVar

-- | A singleton context from a name and list of types.
oneTrait :: Name -> [BaseType] -> Context
oneTrait name = Context . S.singleton . HasTrait name
