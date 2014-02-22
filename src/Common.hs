{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Common ( (!), (<!>), (<$>), (<$), (<*), (*>), (<*>), pure
              , get, modify, put, lift, forM_, forM, when, Monoid(..)
              , (<>), StateT(..), State(..), ErrorT(..), indentBy, Name(..)
              , intercalate, Identity(..), runState, (>>==), trim, line
              , throwError, catchError, (~>), (<$$), Render(..), isInt)  where

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

type Name = String

class Show a => Render a where
  render :: a -> String

instance Render a => Render [a] where
  render as = "[" ++ (intercalate ", " $ map render as) ++ "]"

instance Render a => Render (M.Map Name a) where
  render mp = line $ "{" ++ intercalate ", " pairs ++ "}" where
    pairs = mp ! M.toList ! map (\(n, t) -> n ++ ": " ++ render t)

instance Render a => Render (S.Set a) where
  render set = line $ "{" ++ intercalate ", " elems ++ "}" where
    elems = set ! S.elems ! map render

(!) = flip ($)
infixl 0 !
(<!>) = flip (<$>)
infixl 4 <!>
(~>) = flip (.)
infixl 9 ~>
a <$$ f = pure a <*> f

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
isIntTo :: (Integral a, RealFrac b) => b -> a -> Bool
isIntTo x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

isInt x = isIntTo x 10
