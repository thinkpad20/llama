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
  , Substitution(..), TCState(..), TypeChecker, (\\), oneTrait, oneTrait'
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

-- | A stored trait instance, which might have its own context.
data Instance = Instance Context [BaseType] deriving (Show)
data DefaultEntry = Slot BaseType | Fixed BaseType deriving (Show)
newtype DefaultInstance = DefaultInstance [DefaultEntry]
  deriving (Show)
-- | Stores names that we've typed
type TypeMap = Map Name Polytype
-- | Stores the instances we've made.
type TraitMap = Map Name ([Instance], [DefaultInstance])
-- | A mapping from (type variable) names to BaseTypes.
newtype Substitution = Substitution (Map Name BaseType) deriving (Show, Eq)
-- | Our monadic state. The count is for generating new names.
data TCState = TCState {_count::Int, _tymaps::[TypeMap], _trmaps::[TraitMap]}
-- | The main type checking monad. Ye olde errorT stateT.
type TypeChecker = ErrorT ErrorList (StateT TCState IO)

instance Render Instance where
  render (Instance ctx ts) = render ctx <> ". " <> T.intercalate " " ts' where
    ts' = fmap renderP ts

instance Render DefaultInstance where
  render (DefaultInstance ts) = "default " <> ts' where
    rndr (Slot t) = "<" <> render t <> ">"
    rndr (Fixed t) = render t
    ts' = T.intercalate " " $ fmap rndr ts

instance Render Substitution where
  render (Substitution s) = "{" <> T.intercalate ", " items <> "}" where
    items = fmap (\(f,t) -> render f <> "=>" <> render t) $ H.toList s

instance FreeVars TypeMap where
  freevars = freevars . H.elems

-- | A singleton context with a trait using only type variables.
oneTrait' :: Name -> [Name] -> Context
oneTrait' name = oneTrait name . fmap TVar

-- | A singleton context from a name and list of types.
oneTrait :: Name -> [BaseType] -> Context
oneTrait name = Context . S.singleton . HasTrait name
