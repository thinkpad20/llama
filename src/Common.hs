{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
module Common ( (!), (<!>), (<$>), (<$), (<*), (*>), (<*>), pure
              , get, modify, put, lift, forM_, forM, when, Monoid(..)
              , (<>), StateT(..), State, ErrorT(..), indentBy, Name
              , intercalate, Identity(..), runState, evalState, (>>==)
              , trim, line, throwError, catchError, (~>), Render(..)
              , isInt, ErrorList(..), throwError1, throwErrorC, addError
              , addError', forever, isSpace, catMaybes, sortWith, each
              , unless, mconcatMapM, show, whenM, unlessM, lift2, toList
              , mapM_, fromList)
              where

import Prelude hiding (show, mapM_)
import qualified Prelude as P
import Control.Monad hiding (forM_, mapM_)
import "mtl" Control.Monad.State hiding (forM_, mapM_)
import "mtl" Control.Monad.Error hiding (forM_, mapM_)
import Data.Monoid
import GHC.Exts (sortWith)
import qualified Data.Text as T
import Control.Applicative hiding (many, (<|>))
import Data.List (intercalate)
import "mtl" Control.Monad.Identity hiding (forM_, mapM_)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Sequence
import Data.Foldable
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

instance Render Double
instance Render T.Text
instance Render Int
instance Render Char
instance (Render a, Render b) => Render (a, b) where
  render (a, b) = "(" <> render a <> "," <> render b <> ")"

instance Render String where
  render = P.show ~> T.pack

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

each :: [a] -> (a -> b) -> [b]
each = flip map

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
