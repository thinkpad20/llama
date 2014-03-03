{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Common ( (!), (<!>), (<$>), (<$), (<*), (*>), (<*>), pure
              , get, modify, put, lift, forM_, forM, when, Monoid(..)
              , (<>), StateT(..), State, ErrorT(..), indentBy, Name
              , intercalate, Identity(..), runState, evalState, (>>==)
              , trim, line, throwError, catchError, (~>), (<$$), Render(..)
              , isInt, ErrorList(..), throwError1, throwErrorC, addError
              , addError', forever, isSpace)  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Monoid
import qualified Data.Text as T
import Control.Applicative hiding (many, (<|>))
import Data.List (intercalate)
import Control.Monad.Identity
import Data.Char (isSpace)
import qualified Data.Map as M
import qualified Data.Set as S

newtype ErrorList = TE [T.Text]
instance Error ErrorList where
  strMsg = TE . pure . T.pack

instance Render ErrorList

instance Show ErrorList where
  show (TE msgs) = show msgs
  --show (TE msgs) = msgs ! concatMap ((<> "\n") . indentBy 4 . trim) ! line

type Name = T.Text

class Show a => Render a where
  render :: a -> T.Text
  render = show ~> T.pack
  renderIO :: a -> IO T.Text
  renderIO = return . render

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

(<$$) :: Applicative f => (a -> b) -> f a -> f b
a <$$ f = pure a <*> f

(>>==) :: Monad m => m b -> (b -> m a) -> m b
action1 >>== action2 = action1 >>= \r -> action2 r >> return r

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
isIntTo :: (Integral a, RealFrac b) => b -> Int -> Bool
isIntTo x n = round (10 ^ fromIntegral n * (x- fromIntegral (round x))) == 0

isInt :: RealFrac b => b -> Bool
isInt x = isIntTo x 10

throwErrorC = throwError1 . mconcat
throwError1 = throwError . TE . pure
addError msg (TE msgs) = throwError $ TE $ msg : msgs
addError' = addError . mconcat
