{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module TypeCheckerTests (allTests) where

import qualified Data.Map as M
import qualified Data.Text as T

import Common
import Tests
import AST
import TypeLib
import TypeChecker

(a, b, a0, a1) = (TVar "a", TVar "b", TVar "a0", TVar "a1")
[a', b', a0', a1'] = map (Polytype []) [a, b, a0, a1]

basicTests = TestGroup "Basic expressions" [
    Test "numbers" "1" numT
  , Test "strings" "\"hello\"" strT
  , Test "booleans" "False" boolT
  , Test "booleans 2" "True" boolT
  , Test "variable definitions" "a = 3" numT
  , Test "storing variables" "a = 3; a" numT
  , Test "blocks" "a = 0; 1; \"hello\"; 2; False; a" numT
  ]

functionTests = TestGroup "functions" [
    Test "lambdas" "x => x" (a ==> a)
  , Test "lambdas with type" "x: Str => x" (strT ==> strT)
  , Test "lambdas 3" "x: Str => y: Num => y" (strT ==> numT ==> numT)
  , Test "lambda with tuple" "x: (Str, Str) => x"
         (tTuple [strT, strT] ==> tTuple [strT, strT])
  , Test "lambda with tuple 2" "(x: Str, y: Num) => (y, x)"
         (tTuple [strT, numT] ==> tTuple [numT, strT])
  , Test "lambda with literal in tuple"
         "(x: Str, 2, y: Num) => (y, x)"
         (tTuple [strT, numT, numT] ==> tTuple [numT, strT])
  , Test "lambda with literal in tuple 2"
         "(\"hello\", 2, y: Num) => y"
         (tTuple [strT, numT, numT] ==> numT)
  , Test "polymorphic lambda" "x => Just x" (a ==> maybeT a)
  , Test "polymorphic lambda 2" "x => [x]" (a ==> arrayOf a)
  , Test "polymorphic lambda 2" "x => [x, x]" (a ==> arrayOf a)
  , Test "polymorphic with tuple" "(x, y: Num) => (y, x)"
         (tTuple [a, numT] ==> tTuple [numT, a])
  , Test "polymorphic with function" "f: Num -> b => f 1"
         ((numT ==> a) ==> a)
  , Test "polymorphic with function, definition"
         "foo = f: Num -> b => f 1; foo"
         ((numT ==> a) ==> a)
  , ShouldError "Fails occurs check" "x => x x"
  , Test "recursive function"
         "fact (n: Num) = if n < 2 then 1 else fact (n - 1)"
         (numT ==> numT)
  , Test "recursive function, non-terminating"
         "bottom x = bottom x" (a ==> b)
  , ShouldError "mismatched rigid type variables"
              "foo (f: a -> a) = (); bar (g: a -> b) = foo g"
  ]

ifTests = TestGroup "conditionals" [
    TestGroup "if statements" [
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
    , ShouldError "non-matching results" "if True then 1 else \"hello\""
    , ShouldError "non-matching results 2"
                  "if True then 1 else if False then 2 else \"hello\""
    , ShouldError "non-bool condition" "if 1 then 2 else 3"
  ]
  , TestGroup "for expressions" [
    Test "basic" "for 1; True; 3 do 1" (maybeT numT)
  , Test "basic 2 lines" "for 1; False; 3 {1; \"hello\"}" (maybeT strT)
  ]
  ]

caseTests = TestGroup "case expressions" [
    Test "basic" "case 1 of 2 => 3" numT
  , Test "unified with alternative"
         "foo => case foo of 1 => 2"
         (numT ==> numT)
  ]

appTests = TestGroup "applications" [
    TestGroup "normal function application" [
      test "basic" "foo 1" numT
    ]
  , TestGroup "dotted function application" [
      test "basic" "1.foo" numT
    ]
  ]
  where test desc s = Test desc ("foo (n: Num) = n + 1; " <> s)

binaryTests = TestGroup "binary operators" [
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

vectorTests = TestGroup "vectors" [
    Test "empty" "[]"                  (arrayOf a)
  , Test "basic" "[1,2,3]"             (arrayOf numT)
  , Test "extending" "[1,2,3] + [4,5]" (arrayOf numT)
  , Test "appending" "[1,2,3] + 4"     (arrayOf numT)
  , Test "appending 2" "[] + 4"        (arrayOf numT)
  , Test "prepending" "4 + [1,2,3]"    (arrayOf numT)
  , Test "prepending 2" "4 + []"       (arrayOf numT)
  ]

multifunctionTests = TestGroup "Multifunctions" [
    Test "can extend a function definition"
         ("foo (n: Num) = n + 1; " <>
          "foo (s: Str) &= s + \"!\"; foo")
         (mf [(numT, numT), (strT, strT)])
  , Test "can extend a function definition with tuple"
         ("foo (n: Num) = n + 1; " <>
          "foo (s: Str) &= s + \"!\"; " <>
          "foo (s: Str, n: Num) &= s + \"!\"; foo")
         (mf [(numT, numT), (strT, strT), (tTuple [strT, numT], strT)])
  , Test "can use recursion when extending a function"
          ("foo (s: Str) = 2; " <>
           "foo (n: Num) &= if n < 2 then 1 else foo (n - 1); foo")
          (mf [(numT, numT), (strT, numT)])
  , Test "can use recursion when extending a function 2"
          ("foo (s: Str) = 2; " <>
           "foo (n: Num) &= if n < 2 then foo (show 1) " <>
           "else foo (n - 1); foo")
          (mf [(numT, numT), (strT, numT)])
  , Test "can extend a function definition with generic"
         ("foo (n: Num) = n + 1; " <>
          "foo (x: a) &= [x]; foo")
         (mf [(numT, numT), (a, arrayOf a)])
  , Test "can extend a binary function definition"
       "(n: Num) / (s: Str) &= n + 1; _/_"
       (mf [ (tTuple [numT, numT], numT)
           , (tTuple [numT, strT], numT)])
  , Test "can use a previous definition inside an extension"
       ("foo (n: Num) = n + 1; " <>
        "foo (s: Str) &= (s, foo 3); foo")
        (mf [(numT, numT), (strT, tTuple [strT, numT])])
  , Test "can apply a multifunction" "length \"hello\"" numT
  , Test "can apply a multifunction 2" "length [1,2,3]" numT
  , ShouldError "overwriting multifunction"
                "foo (n: Num) = n + 1; foo (n: Num) &= n - 1; foo"
  , ShouldError "extending with a non-function"
                "foo (n: Num) = n + 1; foo &= 1; foo"
  , ShouldError "ambiguous application"
      "foo (n: Num) = n + 1; foo (s: Str) &= s + \"hello\"; x => foo x"
  ]
  where mf = TMultiFunc . M.fromList

unifyTests1 = TestGroup "Unification" [
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
  , TestGroup "Multifunctions, applied to functions" [
      subs "basic"
           ("{m Num -> Num}", "Num -> Num") []
    , subs "basic 2"
           ("{m Num -> Num, Str -> Num}", "Num -> Num") []
    , subs "with variable"
           ("{m a -> Num, Str -> Num}", "Num -> Num") [("a", numT)]
    , subs "with two variables"
           ("{m a -> b, Str -> Num}", "Num -> Num")
           [("a", numT), ("b", numT)]
    , subs "applied to a variable"
           ("{m Str -> Num}", "a -> Num") [("a", strT)]
    , subs "applied to a variable 2"
           ("{m Str -> Num, Num -> Str}", "a -> Num") [("a", strT)]
    , subs "applied to tuple"
           ("{m (Num, Num) -> Num, (a, [a]) -> [a]}"
           , "(Num, [b]) -> c")
           [("a", numT), ("b", numT), ("c", arrayOf numT)]
    , subs "with applied type"
           ("{m Maybe Num -> Num, Str -> Num}", "Maybe Num -> Num") []
    , subs "with applied type 2"
           ("{m Maybe Num -> Num, Str -> Num}", "Maybe a -> Num") [("a", numT)]
    , subs "with applied type 3"
           ("{m Maybe Num -> b, Str -> Num}", "Maybe a -> Str")
           [("a", numT), ("b", strT)]
    , subs "should pick most specific"
           ("{m a -> b, Str -> Num}", "Str -> Num") []
    ]
  ]
  where
    subs name inputs subs' = Test name inputs (fromList subs')

unifyFailTests = TestGroup "Invalid unifications" [
    ShouldError "non-matching constants" ("Num", "Str")
  , ShouldError "non-matching constant tuples" ("Num", "(Str, Num)")
  , ShouldError "non-matching constant tuples 2" ("(Num, Str)", "(Str, Num)")
  , ShouldError "non-matching constant tuples 3"
                ("([Num], Num)", "([Num], [Num])")
  , ShouldError "non-matching constant function" ("Num", "Str -> Num")
  , ShouldError "non-matching constant function 2" ("Num -> Str", "Str -> Num")
  , ShouldError "non-matching constant function 3"
                ("([Num], Num) -> [Num]", "([Num] [Num]) -> Num")
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
  , ShouldError "multi (m, f) ambiguous"
          ("{m [a] -> b, a -> b, Str -> Num}", "[Str] -> Num")
  ]

instantiationTests = TestGroup "instantiation" [
    Test "constant type" (Polytype [] numT) numT
  , Test "constant type with variable" (Polytype ["foo"] numT) numT
  , Test "constant type with variable 2" (Polytype ["Num"] numT) numT
  , Test "variable type" (Polytype ["a"] a) a0
  , Test "variable type 2"
    (Polytype ["a"] (TApply a b)) (TApply a0 b)
  , Test "two variable types"
    (Polytype ["a", "b"] (TApply a b))
    (TApply a0 a1)
  , Test "with a function"
    (Polytype ["a"] $ numT ==> a) (numT ==> a0)
  , Test "with a tuple"
    (Polytype ["a"] $ tTuple [numT, a]) (tTuple [numT, a0])
  , Test "with a function in a tuple"
    (Polytype ["a"] $ tTuple [numT ==> a, b]) (tTuple [numT ==> a0, b])
  , Test "with a tuple in a function"
    (Polytype ["a", "b"] $ tTuple [numT ==> a, b] ==> strT)
    (tTuple [numT ==> a0, a1] ==> strT)
  ]

generalizationTests = TestGroup "generalization" [
    Test "constant type" (mempty, numT) (Polytype [] numT)
  , Test "variable type" (mempty, a) (Polytype ["a"] a)
  , Test "variable type 2"
    (mempty, TApply a b)
    (Polytype ["a", "b"] (TApply a b))
  , Test "should use names starting with 'a'"
    (mempty, TApply (TVar "blibber") (TVar "blob"))
    (Polytype ["a", "b"] $ TApply a b)
  , Test "variable type"
    (TE $ M.fromList [("x", a0'), ("y", a1')], tTuple [TApply a0 a1, b])
    (Polytype ["a"] $ tTuple [TApply a0 a1, a])
  ]

generalizeE :: (TypeEnv, Type) -> Either ErrorList Polytype
generalizeE (env, t) = Right $ generalize' env t

group1 :: [Test String Type]
group1 = [basicTests, functionTests, ifTests, binaryTests, vectorTests
         , multifunctionTests, caseTests, appTests]

allTests = [ run typeIt group1
           , run unifyIt [unifyTests1, unifyFailTests]
           --, run testInstantiate [instantiationTests]
           , run generalizeE [generalizationTests]
           ]

main = runAllTests allTests
