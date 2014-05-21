{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}
module Common (
    module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Error
  , module Control.Monad.Identity
  , module Control.Monad.State.Strict
  , module Control.Monad.Trans
  , module Control.Monad.Writer
  , module Data.Char
  , module Data.Foldable
  , module Data.HashMap.Strict
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Sequence
  , module Data.String
  , module Data.Text
  , module GHC.Exts
  , module Prelude
  , Render(..), Name
  , ErrorList(..), throwError1, throwErrorC, addError, addError'
  , mconcatMapM, whenM, unlessM, lift2
  , isInt, isIntTo, trim, indentBy, contains, line, each
  , (!), (<!>), (>>==), (~>)) where

import Prelude (IO, Eq(..), Ord(..), Bool(..), tail, Show(..), Char,
                Double, String, Maybe(..), Int, Monad(..), Integer,
                ($), (.), floor, map, Functor(..), mapM, fst, snd,
                (+), (-), Either(..), unwords, flip, head, error,
                fromIntegral, round, (^), (*), putStrLn, map,
                otherwise, length, Read(..), read, (&&), FilePath,
                readFile, print, not)
import qualified Prelude as P
import Control.Applicative
import Control.Monad ((>=>))
import "mtl" Control.Monad.Error (MonadError(..), Error(..), ErrorT(..))
import "mtl" Control.Monad.Identity (Identity(..))
import "mtl" Control.Monad.Trans (liftIO)
import "mtl" Control.Monad.State.Strict (MonadState(..), State(..)
                                        , StateT(..), MonadTrans(..)
                                        , modify
                                        , execState, evalState, lift)
import "mtl" Control.Monad.Writer (MonadWriter(..), WriterT(..))
import Data.Char (isSpace)
import Data.Foldable
import Data.HashMap.Strict hiding (map, (!), toList, fromList, empty
                                  , filter, foldr, foldl', null, adjust
                                  , singleton)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence hiding (replicate, length, empty)
import Data.String
import Data.Text (Text(..), pack, unpack, snoc)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Exts (sortWith)
import Text.Parsec (ParseError)

instance Render a => Render (Seq a) where
  render vec = "[" <> T.intercalate "," (toList $ fmap render vec) <> "]"

newtype ErrorList = ErrorList [T.Text]
instance Error ErrorList where
  strMsg = ErrorList . pure . T.pack

instance Render ErrorList

instance Show ErrorList where
  show (ErrorList msgs) = P.show msgs
  --show (ErrorList msgs) = msgs ! concatMap ((<> "\n") . indentBy 4 . trim) ! line

type Name = T.Text

class Show a => Render a where
  render :: a -> T.Text
  render = P.show ~> T.pack
  renderIO :: a -> IO T.Text
  renderIO = return . render
  pretty :: a -> T.Text
  pretty = render
  renderI' :: a -> State (Int, T.Text) ()
  renderI' _ = return ()
  renderI :: Int -> a -> T.Text
  renderI i a = snd $ execState (renderI' a) (i, "")

instance Render Double
instance Render T.Text
instance Render Int
instance Render Integer
instance Render Char
instance Render ()
instance (Render a, Render b) => Render (a, b) where
  render (a, b) = "(" <> render a <> "," <> render b <> ")"

instance Render String

instance Render a => Render [a] where
  render as = "[" <> T.intercalate ", " (map render as) <> "]"

instance Render a => Render (M.Map Name a) where
  render mp = line $ "{" <> T.intercalate ", " pairs <> "}" where
    pairs = mp ! M.toList ! map (\(n, t) -> n <> ": " <> render t)

instance Render a => Render (S.Set a) where
  render set = line $ "{" <> T.intercalate ", " elems <> "}" where
    elems = set ! S.elems ! map render

(!) :: b -> (b -> c) -> c
(!) = flip ($)
infixl 0 !

(<!>) :: Functor f => f a -> (a -> b) -> f b
(<!>) = flip (<$>)
infixl 4 <!>

(~>) :: forall b c a. (a -> b) -> (b -> c) -> a -> c
(~>) = flip (.)
infixl 9 ~>

(>>==) :: Monad m => m b -> (b -> m a) -> m b
action1 >>== action2 = action1 >>= \r -> action2 r >> return r
infixl 1 >>==

trim :: T.Text -> T.Text
trim = f . f
   where f = T.reverse . T.dropWhile isSpace

indentBy :: Int -> T.Text -> T.Text
indentBy amount str =
  str ! T.lines ! map (T.replicate amount " " <>) ! T.intercalate "\n"

contains :: T.Text -> Char -> Bool
text `contains` char = case T.find (==char) text of
  Nothing -> False
  Just _ -> True

line :: T.Text -> T.Text
line s = if s `contains` '\n' then "\n" <> s else s

--Returns if x is an int to n decimal places
isIntTo :: Double -> Int -> Bool
isIntTo x n = do
  let rounded = fromIntegral (round x :: Int)
  (round (10 ^ n * (x - rounded)) :: Int) == 0

isInt :: Double -> Bool
isInt x = isIntTo x 10

throwErrorC :: (Monad m) => [T.Text] -> ErrorT ErrorList m a
throwErrorC = throwError1 . mconcat

throwError1 :: (Monad m) => T.Text -> ErrorT ErrorList m a
throwError1 = throwError . ErrorList . pure

addError :: (Monad m) => T.Text -> ErrorList -> ErrorT ErrorList m a
addError msg (ErrorList msgs) = throwError $ ErrorList $ msg : msgs

addError' ::  (Monad m) => [T.Text] -> ErrorList -> ErrorT ErrorList m a
addError' = addError . mconcat

each :: Functor f => f a -> (a -> b) -> f b
each = flip fmap

mconcatMapM :: (Monad f, Functor f, Monoid t) => (a -> f t) -> [a] -> f t
mconcatMapM f list = mconcat <$> mapM f list

show :: Show a => a -> T.Text
show s = T.pack $ P.show s

whenM, unlessM :: Monad m => m Bool -> m () -> m ()
whenM test act = test >>= \case
  True -> act
  False -> return ()

unlessM test act = test >>= \case
  True -> return ()
  False -> act

instance Render ParseError

lift2 :: (Monad (t1 m), Monad m, MonadTrans t, MonadTrans t1) =>
         m a -> t (t1 m) a
lift2 = lift . lift
