{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Common ( (!), (<!>), (<$>), (<$), (<*), (*>), (<*>), pure
              , get, modify, put, lift, forM_, forM, when, Monoid(..)
              , (<>), StateT(..), State, ErrorT(..), indentBy, Name
              , intercalate, Identity(..), runState, evalState, (>>==)
              , trim, line, throwError, catchError, (~>), (<$$), Render(..), isInt
              , ErrorList(..), throwError1, throwErrorC, addError, addError'
              , forever)  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Monoid
import Control.Applicative hiding (many, (<|>))
import Data.List (intercalate)
import Control.Monad.Identity
import Data.Char (isSpace)
import qualified Data.Map as M
import qualified Data.Set as S

newtype ErrorList = TE [String]
instance Error ErrorList where
  strMsg = TE . pure
instance Show ErrorList where
  show (TE msgs) = msgs ! concatMap ((++ "\n") . indentBy 4 . trim) ! line

type Name = String

class Show a => Render a where
  render :: a -> String
  renderIO :: a -> IO String
  renderIO = return . render

instance Render a => Render [a] where
  render as = "[" ++ intercalate ", " (map render as) ++ "]"

instance Render a => Render (M.Map Name a) where
  render mp = line $ "{" ++ intercalate ", " pairs ++ "}" where
    pairs = mp ! M.toList ! map (\(n, t) -> n ++ ": " ++ render t)

instance Render a => Render (S.Set a) where
  render set = line $ "{" ++ intercalate ", " elems ++ "}" where
    elems = set ! S.elems ! map render

(!) :: forall b c. b -> (b -> c) -> c
(!) = flip ($)
infixl 0 !

(<!>) :: forall a b (f :: * -> *). Functor f => f a -> (a -> b) -> f b
(<!>) = flip (<$>)
infixl 4 <!>

(~>) :: forall b c a. (a -> b) -> (b -> c) -> a -> c
(~>) = flip (.)
infixl 9 ~>

(<$$) :: forall (f :: * -> *) a b. Applicative f => (a -> b) -> f a -> f b
a <$$ f = pure a <*> f

(>>==) :: forall (m :: * -> *) b a. Monad m => m b -> (b -> m a) -> m b
action1 >>== action2 = action1 >>= \r -> action2 r >> return r

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

indentBy :: Int -> String -> String
indentBy amount str =
  str ! lines ! map (replicate amount ' ' ++) ! intercalate "\n"

line :: String -> String
line s = if '\n' `elem` s then "\n" ++ s else s

--Returns if x is an int to n decimal places
isIntTo :: (Integral a, RealFrac b) => b -> Int -> Bool
isIntTo x n = round (10 ^ fromIntegral n * (x- fromIntegral (round x))) == 0

isInt :: forall b. RealFrac b => b -> Bool
isInt x = isIntTo x 10

throwErrorC = throwError1 . concat
throwError1 = throwError . TE . pure
addError msg (TE msgs) = throwError $ TE $ msg : msgs
addError' = addError . concat