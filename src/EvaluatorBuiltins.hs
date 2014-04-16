{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module EvaluatorBuiltins where

import Prelude (IO, Eq(..), Ord(..), Bool(..)
               , Double, Maybe(..), undefined, Monad(..)
               , ($), Int, (.), (*), Either(..), String
               , fst, (+), (-), (/), (=<<), otherwise, fmap)
import qualified Prelude as P
import qualified Data.HashTable.IO as H
import Control.Monad ((>=>))
import Data.Sequence
import Data.Text
import Data.IORef

import Common hiding (intercalate)
import EvaluatorLib

builtIns :: IO Env
builtIns = H.fromList
  [
    ("println", Builtin bi_println), ("+", Builtin bi_plus)
  , ("-", Builtin bi_minus), ("*", Builtin bi_times), ("/", Builtin bi_divide)
  , ("<", Builtin bi_lt), (">", Builtin bi_gt), ("<=", Builtin bi_leq)
  , (">=", Builtin bi_geq), ("==", Builtin bi_eq), ("!=", Builtin bi_neq)
  , ("negate", Builtin bi_negate), ("incr!", Builtin bi_incr)
  , ("append", Builtin bi_vectorAppend), ("prepend", Builtin bi_vectorPrepend)
  , ("append!", Builtin bi_vectorRefAppend)
  , ("prepend!", Builtin bi_vectorRefPrepend)
  , ("read", Builtin bi_read)
  ]

bi_println :: Builtin
bi_println = ("println", unbox >=> println) where
  println val = lift2 (p val) >> pure unitV
  p (VNumber n) | isInt n = P.print (P.floor n :: Int)
                | otherwise = P.print n
  p (VString s) = P.putStrLn $ unpack s
  p val = print val

bi_negate :: Builtin
bi_negate = ("negate", unbox >=> neg) where
  neg (VNumber n) = pure $ VNumber $ P.negate n
  neg val = numTypeError val

bi_read :: Builtin
bi_read = ("read", read) where
  read (VRef ref) = readRef ref
  read val = typeError "Reference" val

bi_plus, bi_minus, bi_divide, bi_times, bi_eq,
  bi_lt, bi_gt, bi_neq, bi_leq, bi_geq, bi_incr,
  bi_vectorAppend, bi_vectorPrepend,
  bi_vectorRefAppend, bi_vectorRefPrepend :: Builtin
bi_plus = bi_binaryDDD "+" (+)
bi_minus = bi_binaryDDD "-" (-)
bi_times = bi_binaryDDD "*" (*)
bi_divide = bi_binaryDDD "/" (/)
bi_eq = bi_binaryDDB "==" (==)
bi_lt = bi_binaryDDB "<" (<)
bi_gt = bi_binaryDDB ">" (>)
bi_leq = bi_binaryDDB "<=" (<=)
bi_geq = bi_binaryDDB ">=" (>=)
bi_neq = bi_binaryDDB "!=" (/=)

bi_vectorAppend = ("Vector append", unbox >=> f) where
  f (VVector vec) = pure $ Builtin ("Vector append " <> render vec, f' vec)
  f val = typeError "Vector" val
  f' vec val = pure $ VVector $ vec |> val

bi_vectorPrepend = ("Vector prepend", unbox >=> f) where
  f (VVector vec) = pure $ Builtin ("Vector prepend " <> render vec, f' vec)
  f val = typeError "Vector" val
  f' vec val = pure $ VVector $ val <| vec

bi_vectorRefAppend = ("Vector append!", f) where
  f (VRef ref) = pure $ Builtin ("Vector append! with arg", f' ref)
  f val = typeError "Reference" val
  f' ref val = lift2 (readIORef ref) >>= \case
    VVector vec -> writeRef ref $ VVector $ vec |> val
    val' -> typeError "Vector" val'

bi_vectorRefPrepend = ("Vector prepend!", f) where
  f (VRef ref) = pure $ Builtin ("Vector prepend! with arg", f' ref)
  f val = typeError "Reference" val
  f' ref val = lift2 (readIORef ref) >>= \case
    VVector vec -> writeRef ref $ VVector $ val <| vec
    val' -> typeError "Vector" val'

bi_incr = ("incr!", f) where
  f :: Value -> Eval Value
  f (VRef ref) = incr ref
  f val = typeError "reference" val
  incr ref = lift2 (readIORef ref) >>= \case
    VNumber n -> do
      let val = VNumber (n + 1)
      lift2 $ writeIORef ref val
      return val
    val -> throwErrorC ["Expecting a number, not `", render val, "'"]

bi_binaryDDD :: Name -> (Double -> Double -> Double) -> Builtin
bi_binaryDDD name op = (name, unbox >=> f) where
  f (VNumber n) = pure $ Builtin (render n <> name, unbox >=> fN n)
  f (VLocal ref) = deref ref >>= \case
    VNumber n -> pure $ Builtin (render n <> name, unbox >=> fN n)
    val -> numTypeError val
  f val = numTypeError val
  fN n (VNumber n') = pure $ VNumber (op n n')
  fN n (VLocal ref) = fN n =<< deref ref
  fN _ val = numTypeError val

bi_binaryDDB :: Name -> (Double -> Double -> Bool) -> Builtin
bi_binaryDDB name op = (name, unbox >=> f) where
  f (VNumber n) = pure $ Builtin (render n <> name, unbox >=> fN n)
  f (VLocal ref) = deref ref >>= \case
    VNumber n -> pure $ Builtin (render n <> name, unbox >=> fN n)
    val -> numTypeError val
  f val = numTypeError val
  fN n (VNumber n') = pure $ VBool (op n n')
  fN n (VLocal ref) = fN n =<< deref ref
  fN _ val = numTypeError val
