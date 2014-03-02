{-# LANGUAGE OverloadedStrings #-}
module ParserTests (doTests) where

import qualified Data.Map as M
import qualified Data.Text as T
import Prelude hiding (mod)

import Common
import Tests
import AST
import Parser (grab)

expr e = [e]
(foo, bar, baz, qux) = (Var "foo", Var "bar", Var "baz", Var "qux")
(fooT, barT, bazT, quxT) = ( tConst "Foo", tConst "Bar"
                           , tConst "Baz", tConst "Qux")
arrayS exprs = expr $ Array $ ArrayLiteral exprs
arrayE exprs = Array $ ArrayLiteral exprs
rangeS start stop = rangeE start stop ! expr
rangeE start stop = ArrayRange start stop ! Array
ops = [ "+", "*", "-", "/", "^", "%", "<", ">", "<="
      , ">=", "==", "!=", "<|", "|>", "<~", "~>", "||", "&&"]
[ plus, times, minus, divide, expon, mod, lt, gt, leq, geq
 , eq, neq, bAp, fAp, bComp, fComp, _or, _and] = map binary ops
opsFuncs = M.fromList [("+", plus), ("*", times), ("-", minus)
                      , ("/", divide), ("^", expon), ("%", mod)
                      , ("<", lt), (">", gt), ("<=", leq), (">=", geq)
                      , ("==", eq), ("!=", neq), ("|>", fAp), ("<|", bAp)
                      , ("~>", fComp), ("<~", bComp)]
(one, two, three) = (Number 1, Number 2, Number 3)

binOpsTests = [test (T.unpack op) | op <- ops] where
  test op = Test ("can parse `" ++ op ++ "'")
                 ("1 " ++ op ++ " 2")
                 (expr $ binary (T.pack op) one two)

expressionTests = TestGroup "Expressions"
  [
    Test "number" "1" (expr one)
  , Test "variable" "foo" (expr foo)
  , TestGroup "binary operators" ([
      Test "binary" "1 + 2" (expr $ plus one two)
    , Test "binary 2" "1 + foo" (expr $ plus one foo)
    , Test "observes precedence rules 1"
            "foo + bar * baz"
            (expr $ plus foo (times bar baz))
    , Test "observes precedence rules 2"
            "foo * bar + baz"
            (expr $ plus (times foo bar) baz)
    , Test "observes associativity rules 1"
            "foo - bar - baz"
            (expr $ minus (minus foo bar) baz)
    , Test "observes associativity rules 2"
            "foo ^ bar ^ baz"
            (expr $ expon foo (expon bar baz))
    , Test "observes associativity rules 3"
            "foo || bar || baz"
            (expr $ _or foo (_or bar baz))
    , Test "observes associativity rules 4"
            "foo && bar && baz"
            (expr $ _and foo (_and bar baz))
  ] ++ binOpsTests)
  , Test "apply" "foo bar" (expr $ Apply foo bar)
  , Test "apply associates to the left"
          "foo bar baz"
          (expr $ Apply (Apply foo bar) baz)
  , TestGroup "strings" [
      Test "basic" "\"hello\"" (expr $ String "hello")
    , Test "can escape quotes" "\"he said \\\"hello\\\" to me\""
           (expr $ String "he said \"hello\" to me")
    , Test "can escape newlines" "\"foo\\nbar\""
           (expr $ String "foo\nbar")
  ]
  , TestGroup "dot" [
      Test "dot" "foo.bar" (expr $ Dot foo bar)
    , Test "dot associates left"
           "foo.bar.baz"
           (expr $ Dot (Dot foo bar) baz)
    , Test "dot is higher precedence than apply"
           "foo baz.bar"
           (expr $ Apply foo (Dot baz bar))
    , Test "dot is higher precedence than apply 2"
           "foo.baz bar"
           (expr $ Apply (Dot foo baz) bar)
    , Test "dots work on numbers" "1 . foo" (expr $ Dot one foo)
    , Test "dots work on decimals" "2.34 . foo" (expr $ Dot (Number 2.34) foo)
    ]
  , TestGroup "Tuples" [
      Test "tuples" "(foo, bar)" (expr $ Tuple [foo, bar])
    , Test "tuples 2" "(foo, bar, baz)" (expr $ Tuple [foo, bar, baz])
    , Test "empty tuple" "()" (expr $ Tuple [])
    , Test "tuple of 1 is not a tuple" "(foo)" (expr foo)
    , Test "nested tuples" "(foo, (bar, baz))"
            (expr $ Tuple [foo, Tuple [bar, baz]])
    , Test "nested tuples 2" "((bar, baz, qux), foo)"
            (expr $ Tuple [Tuple [bar, baz, qux], foo])
    , Test "tuples with expressions inside" "(foo + 1, bar baz)"
            (expr $ Tuple [plus foo one, Apply bar baz])
    , Test "tuples as arguments" "foo(bar, baz)"
            (expr $ Apply foo $ Tuple [bar, baz])
    , Test "tuples as arguments 2" "foo()bar"
            (expr $ Apply (Apply foo $ Tuple []) bar)
    ]
  ]

arrayTests = TestGroup "Arrays"
  [
    TestGroup "literals" [
      Test "make array literals" "[foo, bar, baz]"
           (arrayS [foo, bar, baz])
    , Test "empty array" "[]" (arrayS [])
    , Test "with complex expressions" "[foo * 1 + bar, baz (qux foo.bar)]"
           (arrayS [ plus (times foo one) bar
                   , Apply baz (Apply qux (Dot foo bar))])
    , Test "nested" "[foo, [bar, baz]]" (arrayS [foo, arrayE [bar, baz]])
    , Test "use as arguments" "foo [1, bar, baz]"
           [Apply foo $ arrayE [one, bar, baz]]
    ]
  , TestGroup "ranges" [
      Test "make array ranges" "[foo..bar]" (rangeS foo bar)
    , Test "nested with array literals" "[foo..[bar, baz]]"
           (rangeS foo (arrayE [bar, baz]))
    ]
  , TestGroup "array dereference" [
      Test "make array reference" "foo{! bar}" [Ref foo bar]
    , Test "can contain arbitrary expressions"
           "foo {! bar + baz qux}"
           [Ref foo (plus bar (Apply baz qux))]
    , Test "should have higher precedence than application"
           "foo bar{!baz}" [Apply foo $ Ref bar baz]
    , Test "should have higher precedence than application 2"
           "(foo bar){!baz}" [Ref (Apply foo bar) baz]
    , Test "should have lower precedence than dots"
           "foo.bar{!baz}" [Ref (Dot foo bar) baz]
    , Test "should have lower precedence than dots 2"
           "foo.(bar{!baz})" [Dot foo (Ref bar baz)]
    , Test "should associate to the left"
           "foo{!bar}{!baz}" [Ref (Ref foo bar) baz]
    ]
  ]

typingTests = TestGroup "Typed expressions"
  [
    Test "typing an identifier" "foo: Foo"
         [Typed foo (tConst "Foo")]
  , Test "typing an identifier with a variable type" "foo: a"
         [Typed foo (TVar Rigid "a")]
  , Test "typing with 2nd order type" "bar: Maybe Foo"
         [Typed bar (maybeT fooT)]
  , Test "typing with 2nd order type using a variable"
         "bar: Maybe a"
         [Typed bar (maybeT $ TVar Rigid "a")]
  , Test "typing with type tuple" "foo: (Foo, Bar)"
         [Typed foo (tTuple [fooT, barT])]
  , Test "a typed tuple" "(foo: Foo, bar: Bar)"
         [Tuple [Typed foo fooT, Typed bar barT]]
  ]

functionTests = TestGroup "Functions"
  [
    TestGroup "Lambdas" [
      Test "should make a lambda"
           "foo => foo + 1"
           (eLambda foo $ plus foo one)
    , Test "should make a lambda with a typed argument"
           "foo: Foo => foo + 1"
           (eLambda (Typed foo fooT) $ plus foo one)
    , Test "should make a lambda with a tuple"
           "(foo: Foo, bar: Bar) => foo bar"
           (eLambda tup1 (Apply foo bar))
    , Test "should make a lambda with a list"
           "[] => [1..10]"
           (eLambda (arrayE []) (rangeE one (Number 10)))
    , Test "should make a lambda with multiple statements"
           "foo => { bar foo; foo + 1}"
           (eLambda foo $ Block [Apply bar foo, plus foo one])
    , Test "should make nested lambdas"
           "foo => bar => foo + bar"
           (eLambda foo (Lambda bar (plus foo bar)))
    , Test "should be able to be used in definitions"
           "foo = bar => bar + 1"
           [Define "foo" (Lambda bar (plus bar one))]
    , Test "should be able to be used in assignments"
           "foo := bar: Baz => qux bar"
           [Assign foo (Lambda (Typed bar bazT) (Apply qux bar))]
    , Test "should be able to string alternatives"
           "1 => 2 | 2 => 3"
           (eLambdas [(one, two), (two, three)])
    , Test "should be able to handle complex lambdas"
           "(1 => 2 | 2 => 3 | foo => bar baz) 3"
           [Apply (Lambda (Var "(arg)") $ Case (Var "(arg)")
                                 [ (one, two)
                                 , (two, three)
                                 , (foo, Apply bar baz)]) three]
    ]
  , TestGroup "Defined functions" [
      Test "should make a function definition"
           "foo(bar: Bar) = bar"
           [Define "foo" $ Lambda (Typed bar barT) bar]
    , Test "should make a function definition with multiple args"
           "foo(bar: Bar) (baz: Baz) = bar + baz"
           [Define "foo" $ Lambda (Typed bar barT)
                         $ Lambda (Typed baz bazT)
                         $ plus bar baz]
    , Test "should make a function definition with polymorphic args"
           "foo(bar: a) (baz: b) = bar baz"
           [Define "foo" $ Lambda (Typed bar (TVar Rigid "a"))
                         $ Lambda (Typed baz (TVar Rigid "b"))
                         $ Apply bar baz]
    , Test "should make a function definition with a symbol"
           "(bar: a) <*> (baz: b) = bar baz"
           [Define "<*>" $ Lambda (Tuple [ Typed bar (TVar Rigid "a")
                                         , Typed baz (TVar Rigid "b")])
                         $ Apply bar baz]
    ]
  ]
  where tup1 = Tuple [Typed foo fooT, Typed bar barT]
        eLambda arg body = expr $ Lambda arg body
        eLambdas abds = expr $ Lambda (Var "(arg)") $ Case (Var "(arg)") abds

assignmentTests = TestGroup "Assignments"
  [
    Test "can make definitions" "foo = bar" [Define "foo" bar]
  , Test "can make assignments" "foo := bar" [Assign foo bar]
  , Test "can make complex definitions" "foo = bar + baz"
         [Define "foo" $ bar `plus` baz]
  , Test "can make complex assignments" "foo bar := baz * qux foo"
         [Assign (Apply foo bar) $ baz `times` Apply qux foo]
  ]

blockTests = TestGroup "Blocks"
  [
    SkipTest "separate expressions into blocks by newline"
          "foo\nbar\nbaz qux"
          [foo, bar, Apply baz qux]
  , Test "separate expressions into blocks by semicolon"
         "foo;bar;baz qux"
         [foo, bar, Apply baz qux]
  ]

flowTests = TestGroup "Program flow"
  [
    Test "return" "return" [Return $ Tuple []]
  , Test "return with expr" "return 3" [Return three]
  , Test "return in a series of statements"
         "foo bar; return 3; baz qux"
         [Apply foo bar, Return three, Apply baz qux]
  ]

ifTests = TestGroup "If statements"
  [
    Test "basic one-line" "if 1 then 2 else 3" [If one two three]
  , Test "basic block" "if 1 { 2 } else { 3 }" [If one two three]
  , Test "basic block" "if 1 { 2 } else { 3 }" [If one two three]
  , Test "basic mixed 1" "if 1 then 2 else { 3 }" [If one two three]
  , Test "basic mixed 2" "if 1 { 2 } else 3" [If one two three]
  , Test "without else" "if 1 { 2 }; if 2 then 3"
    [If' one two, If' two three]
  , Test "without else then with" "if 1 { 2 }; if 2 then 3 else 1"
    [If' one two, If two three one]
  , Test "used in lambda" "n: Num => if n then 2 else 3"
    [Lambda (Typed (Var "n") numT) $ If (Var "n") two three]
  , Test "used in define" "foo (n: Num) = if n then 2 else 3"
    [Define "foo" $ Lambda (Typed (Var "n") numT) $ If (Var "n") two three]
  ]

whileTests = TestGroup "While statements"
 [
    TestGroup "single statements" [
      Test "while block via 'do'" "while foo do bar;"
           [While foo bar]
    , SkipTest "while block via 'do' with following expression"
           "while foo do bar\nbaz"
           [While foo bar, baz]
    , Test "while block via 'do' with following expression, split by semicolon"
           "while foo do bar; baz"
           [While foo bar, baz]
  ]
  , TestGroup "using curly braces" [
      Test "basic" "while foo {2}"
           [While foo two]
    , Test "multiple statements"
           "while foo {2; 3}"
           [While foo $ Block [two, three]]
    , SkipTest "complex statements"
           "while foo {bar 2; baz 3}"
           [While foo $ Block [Apply bar two, Apply baz three]]
    , Test "followed by other statement, using semicolon"
           "while foo { bar }; baz"
           [While foo bar, baz]
    , Test "nested while statements 1"
           "while foo { while bar {3}}"
           [ While foo (While bar three)]
    , Test "nested while statements 2"
           "while foo {2; while bar{ 3}; baz}; qux"
           [While foo $ Block [two, While bar three, baz], qux]
  ]
  , SkipTestGroup "using whitespace" [
      Test "basic" "while foo\n  2"
           [While foo two]
    , Test "multiple statements" "while foo\n  2\n  3"
           [While foo $ Block [two, three]]
    , Test "trailing newline" "while foo\n  2\n"
           [While foo two]
    , Test "binary condition" "while foo < 2\n  bar 3\n"
           [While (lt foo two) $ Apply bar three]
    , Test "followed by other statement"
           "while foo\n  bar\nbaz"
           [While foo bar, baz]
    , Test "followed by two other statements"
           "while foo\n  bar\nbaz\nqux"
           [While foo bar, baz, qux]
    , Test "nested while statements 1"
           "while foo\n  while bar\n    3\n"
           [ While foo $ While bar three]
    , Test "nested while statements 2"
           "while foo\n  2\n  while bar\n    3\n  baz"
           [While foo $ Block [two, While bar three, baz]]
    ]
  , SkipTestGroup "using both" [
         Test "followed by other statement"
         "while foo { bar }\nbaz"
         [While foo bar, baz]

    ]
  ]

forTests = TestGroup "For statements" tests where
  tests =
    [
      Test "basic" "for foo in bar { baz }" [For foo bar baz]
    , Test "basic one-line" "for foo in bar do baz" [For foo bar baz]
    , Test "nested via one-line" "for foo in bar do for baz in qux do 1"
      [For foo bar (For baz qux one)]
    ]

doTests = runTests grab [ expressionTests
                        , assignmentTests
                        , arrayTests
                        , blockTests
                        , whileTests
                        , forTests
                        , typingTests
                        , functionTests
                        , ifTests
                        , flowTests
                        ]

main = doTests
