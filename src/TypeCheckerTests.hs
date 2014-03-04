{-# LANGUAGE OverloadedStrings #-}
module TypeCheckerTests (doTests) where

import qualified Data.Map as M
import qualified Data.Text as T

import Common
import Tests
import AST
import TypeChecker (typeIt)


basicTests = TestGroup "Basic expressions" [
    Test "numbers" "1" numT
  , Test "strings" "\"hello\"" strT
  , Test "booleans" "False" boolT
  , Test "booleans 2" "True" boolT
  , Test "variables" "a = 3; a" numT
  , Test "lambdas" "x: Num => x" (numT ==> numT)
  , Test "lambdas 2" "x: Str => x" (strT ==> strT)
  , TestGroup "if expressions" [
      Test "basic" "if False then 1 else 2" numT
    , ShouldError "non-bool condition" "if 1 then 2 else 3"
  ]
  --, TestGroup "binary operators"
  ]

doTests = runTests typeIt [basicTests]

main = doTests
