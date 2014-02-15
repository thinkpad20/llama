{-# LANGUAGE NoMonomorphismRestriction #-}
module Common ( (!), (<!>), (<$>), (<$), (<*), (*>), (<*>)
              , get, modify, put, lift, forM_, when, Monoid(..)
              , (<>), StateT(..), State(..), ErrorT(..)
              , intercalate, Identity(..), runState)  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Monoid
import Control.Applicative hiding (many, (<|>))
import Data.List (intercalate)
import Control.Monad.Identity


(!) = flip ($)
infixl 0 !
(<!>) = flip (<$>)
infixl 4 <!>
