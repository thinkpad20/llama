{-# LANGUAGE OverloadedStrings #-}
module TypeCheckerTests (tests) where

import qualified Data.Map as M
import qualified Data.Text as T

import Common
import Tests
import AST
import TypeChecker (typeIt, unifyIt)


basicTests = TestGroup "Basic expressions" [
    Test "numbers" "1" numT
  , Test "strings" "\"hello\"" strT
  , Test "booleans" "False" boolT
  , Test "booleans 2" "True" boolT
  , Test "variable definitions" "a = 3" numT
  , Test "variables" "a = 3; a" numT
  , Test "blocks" "a = 0; 1; \"hello\"; 2; False; a" numT
  , TestGroup "if expressions" [
      Test "basic" "if False then 1 else 2" numT
    , Test "variable" "a = False; if a then 1 else 2" numT
    , ShouldError "non-bool condition" "if 1 then 2 else 3"
    , Test "nested" "if True then 0 else if False then 1 else 2" numT
    , Test "nested 2"
           "if True then if False then 1 else 2 else 3"
           numT
    , Test "nested 3"
           "if (if False then True else False) then 1 else 2"
           numT
  ]
  , TestGroup "binary operators" [
      TestGroup "numbers" [
        Test "plus" "1 + 2" numT
      , Test "minus" "1 - 2" numT
      , Test "times" "1 * 2" numT
      , Test "divide" "1 / 2" numT
      , Test "equal" "1 == 2" boolT
      , Test "not equal" "1 != 2" boolT
      , Test "greater" "1 > 2" boolT
      , Test "less" "1 < 2" boolT
      , Test "gequal" "1 >= 2" boolT
      , Test "lequal" "1 <= 2" boolT
      , Test "in assignment" "foo = 1 <= 2; foo" boolT
      , Test "in a lambda" "foo = x: Num => y: Num => x + y; foo"
             (numT ==> numT ==> numT)
      ]
    , TestGroup "strings" [
        Test "plus" "\"hello\" + \"world\"" strT
      , Test "times number" "\"hello\" * 10" strT
      , Test "equal" "\"hello\" == \"hello\"" boolT
      , Test "not equal" "\"hello\" != \"goodbye\"" boolT
    ]
  ]
  , TestGroup "vectors" [
      Test "basic" "[1,2,3]" (arrayOf numT)
    , Test "extending" "[1,2,3] + [4,5]" (arrayOf numT)
    , Test "appending" "[1,2,3] + 4" (arrayOf numT)
  ]
  , TestGroup "functions" [
      Test "lambdas" "x: Num => x" (numT ==> numT)
    , Test "lambdas 2" "x: Str => x" (strT ==> strT)
    , Test "lambdas 3" "x: Str => y: Num => y" (strT ==> numT ==> numT)
    , Test "unary op" "foo (x: Bool) = !x; foo" (boolT ==> boolT)
    , Test "unary op 2" "foo (x: Num) = x !; foo" (numT ==> numT)
    , Test "lambda with tuple" "x: (Str, Str) => x"
           (tTuple [strT, strT] ==> tTuple [strT, strT])
  ]
  ]

unifyTests1 = TestGroup "Basic unification" [
    subs "constants" ("Num", "Num") []
  , subs "constants in tuples" ("(Num, Num)", "(Num, Num)") []
  , subs "applied types" ("Maybe Num", "Maybe Num") []
  , subs "functions" ("Num -> Str", "Num -> Str") []
  , subs "functions with tuples"
         ("Num -> (Str, Str)", "Num -> (Str, Str)") []
  , subs "functions with tuples 2"
         ("(Str, Str, Num) -> (Num, Str)", "(Str, Str, Num) -> (Num, Str)") []
  , subs "variable" ("Num", "a") [("a", numT)]
  , subs "variable in tuple" ("(Num, Num)", "(Num, a)") [("a", numT)]
  , subs "variable in tuple 2" ("(a, Num)", "(Str, Num)") [("a", strT)]
  , subs "2 variables in tuple" ("(a, b)", "(Str, Num)")
         [("a", strT), ("b", numT)]
  , subs "2 variables in tuple 2" ("(Str, Num)", "(a, b)")
         [("a", strT), ("b", numT)]
  , subs "variables in nested tuple" ("(Str, (Num, Str))", "(a, b)")
         [("a", strT), ("b", tTuple [numT, strT])]
  , subs "variables in nested tuple 2" ("(a, (Num, Str))", "(Str, b)")
         [("a", strT), ("b", tTuple [numT, strT])]
  , subs "variables in functions" ("a", "Num -> Str") [("a", numT ==> strT)]
  , subs "variables in functions 2" ("a -> Str", "Num -> Str") [("a", numT)]
  , subs "variables in functions 3" ("a -> b", "Num -> Str")
         [("a", numT), ("b", strT)]
  , subs "variables in applied types" ("Maybe a", "Maybe Num") [("a", numT)]
  , subs "variables in applied types 2" ("a Num", "Maybe Num")
         [("a", TConst "Maybe")]
  , subs "variables in applied types 3" ("a b", "Maybe Num")
         [("a", TConst "Maybe"), ("b", numT)]
  , subs "multi (m, f) basic"
         ("{m Num -> Num}", "Num -> Num") []
  , subs "multi (m, f) basic 2"
         ("{m Num -> Num, Str -> Num}", "Num -> Num") []
  , subs "multi (m, f) with variable"
         ("{m a -> Num, Str -> Num}", "Num -> Num") [("a", numT)]
  , subs "multi (m, f) with two variables"
         ("{m a -> b, Str -> Num}", "Num -> Num")
         [("a", numT), ("b", numT)]
  , subs "multi (m, f) with applied type"
         ("{m Maybe Num -> Num, Str -> Num}", "Maybe Num -> Num") []
  , subs "multi (m, f) with applied type 2"
         ("{m Maybe Num -> Num, Str -> Num}", "Maybe a -> Num") [("a", numT)]
  , subs "multi (m, f) with applied type 3"
         ("{m Maybe Num -> b, Str -> Num}", "Maybe a -> Str")
         [("a", numT), ("b", strT)]
  , subs "multi (m, f) should pick most specific"
         ("{m a -> b, Str -> Num}", "Str -> Num") []
  ]
  where
    subs name inputs subs' = Test name inputs (M.fromList subs')

unifyFailTests = TestGroup "Invalid unifications" [
    ShouldError "non-matching constants" ("Num", "Str")
  , ShouldError "non-matching constant tuples" ("Num", "(Str, Num)")
  , ShouldError "non-matching constant tuples 2" ("(Num, Str)", "(Str, Num)")
  , ShouldError "non-matching constant function" ("Num", "Str -> Num")
  , ShouldError "non-matching constant function 2" ("Num -> Str", "Str -> Num")
  , ShouldError "non-matching variable tuples" ("(Str, a)", "(Num, Str)")
  , ShouldError "non-matching variable tuples 2" ("(a, Num)", "(Num, Str)")
  , ShouldError "non-matching variable tuples 3" ("(a, b, Str)", "(Num, Str)")
  , ShouldError "non-matching variable function" ("Str -> a", "Num -> Str")
  , ShouldError "non-matching variable function 2" ("a -> Num", "Num -> Str")
  , ShouldError "non-matching variable function 3" ("a -> b -> Str", "Num -> Str")
  , ShouldError "multi (m, f) no matches"
          ("{m Foo -> Bar, Str -> Num}", "Num -> Num")
  , ShouldError "multi (m, f) no matches 2"
          ("{m Num -> Num, Str -> Num}", "Num -> Num -> Num")
  , ShouldError "multi (m, f) ambiguous"
          ("{m Num -> Num, Str -> Num}", "a -> Num")
  ]

tests = do res1 <- runTests typeIt [basicTests]
           runTestsWith unifyIt [unifyTests1, unifyFailTests] res1
