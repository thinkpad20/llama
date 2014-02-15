module ParserTests where

import Tests
import Parser

expr e = [Expr e]
(foo, bar, baz, qux) = (Id "foo", Id "bar", Id "baz", Id "qux")

expressionTests = [
    Test "number" "1" (expr $ Number 1)
  , Test "variable" "foo" (expr foo)
  , Test "binary" "1 + 2" (expr $ Binary "+" (Number 1, Number 2))
  , Test "binary 2" "1 + foo" (expr $ Binary "+" (Number 1, foo))
  , Test "observes precedence rules 1"
          "foo + bar * baz"
          (expr $ Binary "+" (foo, Binary "*" (bar, baz)))
  , Test "observes precedence rules 2"
          "foo * bar + baz"
          (expr $ Binary "+" (Binary "*" (foo, bar), baz))
  , Test "observes associativity rules 1"
          "foo - bar - baz"
          (expr $ Binary "-" (Binary "-" (foo, bar), baz))
  , Test "observes associativity rules 2"
          "foo ^ bar ^ baz"
          (expr $ Binary "^" (foo, Binary "^" (bar, baz)))
  , Test "apply" "foo bar" (expr $ Apply foo bar)
  , Test "apply associates to the left"
          "foo bar baz"
          (expr $ Apply (Apply foo bar) baz)
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
    , Test "dots work on numbers" "1 . foo" (expr $ Dot (Number 1) foo)
    , Test "dots work on decimals" "2.34 . foo" (expr $ Dot (Number 2.34) foo)
    ]
  , TestGroup "Tuples" $ [
      Test "tuples" "(foo, bar)" (expr $ Tuple [foo, bar])
    , Test "tuples 2" "(foo, bar, baz)" (expr $ Tuple [foo, bar, baz])
    , Test "empty tuple" "()" (expr $ Tuple [])
    , Test "tuple of 1 is not a tuple" "(foo)" (expr foo)
    , Test "nested tuples" "(foo, (bar, baz))"
            (expr $ Tuple [foo, Tuple [bar, baz]])
    , Test "nested tuples 2" "((bar, baz, qux), foo)"
            (expr $ Tuple [Tuple [bar, baz, qux], foo])
    , Test "tuples with expressions inside" "(foo + 1, bar baz)"
            (expr $ Tuple [Binary "+" (foo, Number 1), Apply bar baz])
    , Test "tuples as arguments" "foo(bar, baz)"
            (expr $ Apply foo $ Tuple [bar, baz])
    , Test "tuples as arguments 2" "foo()bar"
            (expr $ Apply (Apply foo $ Tuple []) bar)
    ]
  ]

assignmentTests = [
    Test "can make definitions" "foo = bar" [Define foo $ expr bar]
  , Test "can make assignments" "foo := bar" [Assign foo $ expr bar]
  , Test "can make complex definitions" "foo = bar + baz"
         [Define foo $ expr $ Binary "+" (bar, baz)]
  , Test "can make complex definitions 2" "foo bar := baz * qux foo"
         [Assign (Apply foo bar) $ expr $ Binary "*" (baz, Apply qux foo)]
  ]

blockTests = [
    Test "separate expressions into blocks by newline"
          "foo\nbar\nbaz qux"
          [Expr foo, Expr bar, Expr $ Apply baz qux]
  , Test "separate expressions into blocks by semicolon"
         "foo;bar;baz qux"
         [Expr foo, Expr bar, Expr $ Apply baz qux]
  ]

whileTests = [
    TestGroup "single statements" [
      Test "while block via 'do'" "while foo do bar;"
           [While foo (expr bar)]
    , Test "while block via 'do' with following expression"
           "while foo do bar\nbaz"
           [While foo (expr bar), Expr baz]
    , Test "while block via 'do' with following expression, split by semicolon"
           "while foo do bar; baz"
           [While foo (expr bar), Expr baz]
  ]
  , TestGroup "using curly braces" [
      Test "basic" "while foo {2}"
           [While foo (expr $ Number 2)]
    , Test "multiple statements"
           "while foo {2; 3}"
           [While foo [Expr $ Number 2, Expr $ Number 3]]
    , SkipTest "complex statements"
           "while foo {bar 2; baz 3}"
           [While foo [Expr $ Apply bar $ Number 2, Expr $ Apply baz $ Number 3]]
    , Test "followed by other statement"
           "while foo { bar }\nbaz"
           [While foo $ expr bar, Expr baz]
    , Test "followed by other statement, using semicolon"
           "while foo { bar }; baz"
           [While foo $ expr bar, Expr baz]
    , Test "nested while statements 1"
           "while foo { while bar {3}}"
           [ While foo [While bar $ expr $ Number 3]]
    , Test "nested while statements 2"
           "while foo {2; while bar{ 3}; baz}"
           [While foo [Expr $ Number 2
                      , While bar $ expr $ Number 3
                      , Expr baz]]
  ]
  , SkipTestGroup "using whitespace" [
      Test "basic" "while foo\n  2"
           [While foo (expr $ Number 2)]
    , Test "multiple statements" "while foo\n  2\n  3"
           [While foo [Expr $ Number 2, Expr $ Number 3]]
    , Test "trailing newline" "while foo\n  2\n"
           [While foo (expr $ Number 2)]
    , Test "binary condition" "while foo < 2\n  bar 3\n"
           [While (Binary "<" (foo, Number 2)) (expr $ Apply bar (Number 3))]
    , Test "followed by other statement"
           "while foo\n  bar\nbaz"
           [While foo $ expr bar, Expr baz]
    , Test "followed by two other statements"
           "while foo\n  bar\nbaz\nqux"
           [While foo $ expr bar, Expr baz, Expr qux]
    , Test "nested while statements 1"
           "while foo\n  while bar\n    3\n"
           [ While foo [While bar $ expr $ Number 3]]
    , Test "nested while statements 2"
           "while foo\n  2\n  while bar\n    3\n  baz"
           [While foo [Expr $ Number 2
                    , While bar $ expr $ Number 3
                    , Expr baz]]
    ]
  ]

forTests = TestGroup "For statements" tests where
  tests =
    [
      Test "basic" "for foo in bar { baz }" [For foo bar [Expr baz]]
    , Test "basic one-line" "for foo in bar do baz" [For foo bar [Expr baz]]
    , Test "nested via one-line" "for foo in bar do for baz in qux do 1"
      [For foo bar [For baz qux $ expr $ Number 1]]
    ]

main = runTests grab [ TestGroup "Expressions" expressionTests
                     , TestGroup "Assignments" assignmentTests
                     , TestGroup "Blocks" blockTests
                     , TestGroup "While statements" whileTests
                     , forTests ]
