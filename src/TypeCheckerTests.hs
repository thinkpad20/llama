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
  , Test "variable definitions" "a = 3" numT
  , Test "variables" "a = 3; a" numT
  , Test "blocks" "a = 0; 1; \"hello\"; 2; False; a" numT
  , Test "lambdas" "x: Num => x" (numT ==> numT)
  , Test "lambdas 2" "x: Str => x" (strT ==> strT)
  , TestGroup "if expressions" [
      Test "basic" "if False then 1 else 2" numT
    , Test "variable" "a = False; if a then 1 else 2" numT
    , ShouldError "non-bool condition" "if 1 then 2 else 3"
    , Test "nested" "if True then 0 else if False then 1 else 2" numT
    , Test "nested 2"
           "if True then if False then 1 else 2 else 3"
           numT
    , Test "nested 3"
           "if (if False then True Else False) then 1 else 2"
           numT
  ]
  , TestGroup "binary operators" [

  ]
  ]

doTests = runTests typeIt [basicTests]

main = doTests
