module ParserTests where

import Common
import Tests
import AST
import Parser (grab)

expr e = [Expr e]
(foo, bar, baz, qux) = (Var "foo", Var "bar", Var "baz", Var "qux")
(fooT, barT, bazT, quxT) = ( TConst "Foo", TConst "Bar"
                           , TConst "Baz", TConst "Qux")
arrayS exprs = expr $ Array $ ArrayLiteral exprs
arrayE exprs = Array $ ArrayLiteral exprs
rangeS start stop = ArrayRange start stop ! Array ! expr
plus a b = Binary "+" (a, b)
times a b = Binary "*" (a, b)
(one, two, three) = (Number 1, Number 2, Number 3)

expressionTests = TestGroup "Expressions" 
  [
    Test "number" "1" (expr $ one)
  , Test "variable" "foo" (expr foo)
  , Test "binary" "1 + 2" (expr $ plus one two)
  , Test "binary 2" "1 + foo" (expr $ plus one foo)
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
    , Test "dots work on numbers" "1 . foo" (expr $ Dot one foo)
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
            (expr $ Tuple [Binary "+" (foo, one), Apply bar baz])
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
           (arrayS [ Binary "+" (Binary "*" (foo, one), bar)
                , Apply baz (Apply qux (Dot foo bar))])
    , Test "nested" "[foo, [bar, baz]]" (arrayS [foo, arrayE [bar, baz]])
    , Test "use as arguments" "foo [1, bar, baz]"
           (expr $ Apply foo $ arrayE [one, bar, baz])
    ]
  , TestGroup "ranges" [
      Test "make array ranges" "[foo..bar]" (rangeS foo bar)
    , Test "nested with array literals" "[foo..[bar, baz]]"
           (rangeS foo (arrayE [bar, baz]))
    ]
  , TestGroup "array dereference" [
      Test "make array reference" "foo{! bar}" (expr $ Ref foo bar)
    , Test "can contain arbitrary expressions"
           "foo {! bar + baz qux}" 
           (expr $ Ref foo (Binary "+" (bar, Apply baz qux)))
    , Test "should have higher precedence than application"
           "foo bar{!baz}" (expr $ Apply foo $ Ref bar baz)
    , Test "should have higher precedence than application 2"
           "(foo bar){!baz}" (expr $ Ref (Apply foo bar) baz)
    , Test "should have lower precedence than dots"
           "foo.bar{!baz}" (expr $ Ref (Dot foo bar) baz)
    , Test "should have lower precedence than dots 2"
           "foo.(bar{!baz})" (expr $ Dot foo (Ref bar baz))
    , Test "should associate to the left"
           "foo{!bar}{!baz}" (expr $ Ref (Ref foo bar) baz)
    ]
  ]

typingTests = TestGroup "Typed expressions"
  [
    Test "typing an identifier" "foo: Foo"
         (expr $ Typed foo (TConst "Foo"))
  , Test "typing with 2nd order type" "bar: Option Foo"
         (expr $ Typed bar (TApply (TConst "Option") (TConst "Foo")))
  , Test "typing with type tuple" "foo: (Foo, Bar)"
         (expr $ Typed foo (TTuple [fooT, barT]))
  , Test "a typed tuple" "(foo: Foo, bar: Bar)"
         (expr $ Tuple [Typed foo fooT, Typed bar barT])
  ]

functionTests = TestGroup "Functions"
  [
    TestGroup "Lambdas" [
      Test "should make a lambda"
           "foo=> foo + 1"
           (eLambda foo $ expr $ Binary "+" (foo, one))
    , Test "should make a lambda with a typed argument"
           "foo: Foo => foo + 1"
           (eLambda (Typed foo fooT) $ expr $ Binary "+" (foo, one))
    , Test "should make a lambda with a tuple"
           "(foo: Foo, bar: Bar) => foo bar"
           (eLambda tup1 (expr $ Apply foo bar))
    , Test "should make a lambda with a list"
           "[] => [1..10]"
           (eLambda (arrayE []) (rangeS one (Number 10)))
    , Test "should make a lambda with multiple statements"
           "foo => { bar foo; foo + 1}"
           (eLambda foo [Expr $ Apply bar foo, Expr $ Binary "+" (foo, one)])
    , Test "should make nested lambdas"
           "foo => bar => foo + bar"
           (eLambda foo (eLambda bar (expr $ plus foo bar)))
    , Test "should be able to be used in definitions"
           "foo = bar => bar + 1"
           [Define foo (eLambda bar (expr $ plus bar $ one))]
    , Test "should be able to be used in assignments"
           "foo := bar: Baz => qux bar"
           [Assign foo (eLambda (Typed bar bazT) (expr $ Apply qux bar))]
    , Test "should be able to string alternatives"
           "1 => 2 | 2 => 3"
           (eLambdas [(one, expr two), (two, expr three)])
    , Test "should be able to handle complex lambdas"
           "(1 => 2 | 2 => 3 | foo => bar baz) 3"
           (expr $ Apply (Lambda [ (one, expr two)
                                 , (two, expr three)
                                 , (foo, expr $ Apply bar baz)]) three)
    ]
  ]
  where tup1 = Tuple [Typed foo fooT, Typed bar barT]
        eLambda arg body = expr $ Lambda [(arg, body)]
        eLambdas = expr . Lambda

assignmentTests = TestGroup "Assignments" 
  [
    Test "can make definitions" "foo = bar" [Define foo $ expr bar]
  , Test "can make assignments" "foo := bar" [Assign foo $ expr bar]
  , Test "can make complex definitions" "foo = bar + baz"
         [Define foo $ expr $ bar `plus` baz]
  , Test "can make complex assignments" "foo bar := baz * qux foo"
         [Assign (Apply foo bar) $ expr $ baz `times` Apply qux foo]
  ]

blockTests = TestGroup "Blocks" 
  [
    SkipTest "separate expressions into blocks by newline"
          "foo\nbar\nbaz qux"
          [Expr foo, Expr bar, Expr $ Apply baz qux]
  , Test "separate expressions into blocks by semicolon"
         "foo;bar;baz qux"
         [Expr foo, Expr bar, Expr $ Apply baz qux]
  ]

whileTests = TestGroup "While statements" 
 [
    TestGroup "single statements" [
      Test "while block via 'do'" "while foo do bar;"
           [While foo (expr bar)]
    , SkipTest "while block via 'do' with following expression"
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
  , SkipTestGroup "using both" [
         Test "followed by other statement"
         "while foo { bar }\nbaz"
         [While foo $ expr bar, Expr baz]

    ]
  ]

forTests = TestGroup "For statements" tests where
  tests =
    [
      Test "basic" "for foo in bar { baz }" [For foo bar [Expr baz]]
    , Test "basic one-line" "for foo in bar do baz" [For foo bar [Expr baz]]
    , Test "nested via one-line" "for foo in bar do for baz in qux do 1"
      [For foo bar [For baz qux $ expr $ one]]
    ]

main = runTests grab [ expressionTests
                     , assignmentTests
                     , arrayTests 
                     , blockTests
                     , whileTests
                     , forTests
                     , typingTests
                     , functionTests
                     ]
