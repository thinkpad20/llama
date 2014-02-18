{-# LANGUAGE NoMonomorphismRestriction #-}
module Common ( (!), (<!>), (<$>), (<$), (<*), (*>), (<*>), pure
              , get, modify, put, lift, forM_, forM, when, Monoid(..)
              , (<>), StateT(..), State(..), ErrorT(..), indentBy
              , intercalate, Identity(..), runState, (>>==), trim, line
              , throwError, catchError, (~>), (<$$), Render(..))  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Monoid
import Control.Applicative hiding (many, (<|>))
import Data.List (intercalate)
import Control.Monad.Identity
import Data.Char (isSpace)


class Render a where
  render :: a -> String

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
