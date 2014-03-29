{-# LANGUAGE OverloadedStrings #-}
module DesugarTests where

import Prelude ()
import Tests
import Desugar
import Parser (grabOrError)


lambdasTests = TestGroup "Lambdas"
  [
    test "basic"
         "1 => 2 | 3 => 4"
         "_arg => case _arg of 1 => 2 | 3 => 4"
  ] where test desc input expect =
            Test desc input (grabOrError expect)

main = runTests desugarIt [lambdasTests]
