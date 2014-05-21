{-# LANGUAGE OverloadedStrings #-}
module DesugarTests where

import Prelude ()
import Tests
import Desugar
import AST
import Parser (grabOrError)

testWith desugarer desc input expect =
  Test desc (desugarer, input) (grabOrError expect)

testAll desc input expect =
  Test desc input (grabOrError expect)

lambdasTests = TestGroup "Lambdas"
  [
    test "basic" "1 => 2 | 3 => 4"
         "_arg => case _arg of 1 => 2 | 3 => 4"
  , test "in assignment"
         "foo = 1 => 2 | 3 => 4"
         "foo = _arg => case _arg of 1 => 2 | 3 => 4"
  , test "in assignment with multiple args"
         "foo bar = 1 => 2 | 3 => 4"
         "foo bar = _arg => case _arg of 1 => 2 | 3 => 4"
  , test "applied"
         "(1 => 2 | 3 => 4) foo"
         "(_arg => case _arg of 1 => 2 | 3 => 4) foo"
  ] where test = testWith dsLambdas

afterBeforeTests = TestGroup "After/Before"
  [
    test "after with single"
         "foo after bar"
         "bar; foo"
  , test "after with block"
         "foo after {bar; baz}"
         "bar; baz; foo"
  , Test "before with single"
         (dsAfterBefore, "foo before bar")
         (Block [Define "_var" (Var "foo"), Var "bar", Var "_var"])
  , Test "before with block"
         (dsAfterBefore, "foo before {bar; baz}")
         (Block [Define "_var" (Var "foo"), Var "bar", Var "baz", Var "_var"])
  , test "inside of a function"
         "foo n = result after {result = 0; println result}"
         "foo n = {result = 0; println result; result}"
  ] where test = testWith dsAfterBefore

lambdaDotTests = TestGroup "LambdaDot"
  [
    test "basic" ".foo" "_arg => _arg.foo"
  , test "applied to other" ".foo bar" "_arg => _arg.foo bar"
  , test "dotted with other" "(.foo).bar" "(_arg => _arg.foo).bar"
  , test "applied to two" ".bar 1 2" "_arg => _arg.bar 1 2"
  , test "nested" ".foo (.bar)" "_arg => _arg.foo (_arg0 => _arg0.bar)"
  ] where test = testWith dsLambdaDot

prefixTests = TestGroup "Prefix notation"
  [
    test "basic" "1; +2" "1 + 2"
  , test "multi" "1; +2; *3; /5; - 6" "(((1 + 2) * 3)/5) - 6"
  , test "more complex" "Just 1; |> .bind incr" "Just 1 |> .bind incr"
  ] where test = testWith dsPrefixLine

dotTests = TestGroup "Dotted expressions"
  [
    test "basic" "foo.bar" "bar foo"
  , test "on the right" "foo bar.baz" "foo (baz bar)"
  , test "on the left" "foo.bar baz" "(bar foo) baz"
  , test "multi" "foo.bar.baz" "baz (bar foo)"
  , test "multi applications" "foo.bar baz qux" "bar foo baz qux"
  ] where test = testWith dsDot

multiTests = TestGroup "Multiple desugarers"
  [
    testAll "faux do-block" "Just 1; |> .bind incr"
            "Just 1 |> _arg => bind _arg incr"
  ]

singles = [ lambdasTests, afterBeforeTests, lambdaDotTests
          , prefixTests, dotTests]
multis = [multiTests]

allTests = [ run desugarIt' singles
           , run desugarIt multis ]
