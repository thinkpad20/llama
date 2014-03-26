{-# LANGUAGE PackageImports #-}
module EvalLib where

import Prelude (IO, Show(..), Eq(..), Ord(..), Bool(..), Double)
import qualified Data.HashTable.IO as H
import Data.HashMap
import "hashmap" Data.HashSet
import Data.Text
import qualified Data.Vector.Persistent as V
import qualified Data.Array.IO as A

import Common
import AST
import TypeLib

data Value = VPrim Prim
           | VObj Obj
           deriving (Show)

type Array a = A.IOArray a

type HashTable k v = H.BasicHashTable k v
type Env = HashTable Name Value
type PMap = Map Value Value
type MMap = HashTable Value Value
type PSet = Set Value
type MSet = HashTable Value Bool

data Prim = Num !Double
          | Str !Text
          | Vector !(V.Vector Value) -- persistent vector
          | PMap !PMap
          | MMap !MMap
          | PSet !PSet
          | MSet !MSet
          | Closure !Expr !Env
          deriving (Show)

data Obj = Obj
  {
    outerType :: !Type
  , attribs :: !(V.Vector Value)
  }
  deriving (Show)

new :: IO Env
new = H.new
