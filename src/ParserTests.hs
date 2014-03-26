{-# LANGUAGE OverloadedStrings #-}
module ParserTests (doTests) where

import qualified Data.Map as M
import qualified Data.Text as T
import Prelude hiding (mod)

import Common
import Tests
import AST
import TypeLib
import Parser (grab)

expr e = [e]
(foo, bar, baz, qux) = (Var "foo", Var "bar", Var "baz", Var "qux")
[fooC, barC, bazC, quxC] = map Constructor ["Foo", "Bar", "Baz", "Qux"]
(fooT, barT, bazT, quxT) = ( tConst "Foo", tConst "Bar"
                           , tConst "Baz", tConst "Qux")
print_ = Apply (Var "print")
assert = Apply (Var "assert")
arrayS exprs = expr $ Literal $ ArrayLiteral exprs
listS exprs = expr $ Literal $ ListLiteral exprs
setS exprs = expr $ Literal $ SetLiteral exprs
dictS exprs = expr $ Literal $ DictLiteral exprs
arrayE exprs = Literal $ ArrayLiteral exprs
rangeS start stop = rangeE start stop ! expr
rangeE start stop = ArrayRange start stop ! Literal
ops = [ "+", "*", "-", "/", "^", "%", "<", ">", "<="
      , ">=", "==", "!=", "$", "|>", "<~", "~>", "||", "&&"]
[ plus, times, minus, divide, expon, mod, lt, gt, leq, geq
 , eq, neq, bAp, fAp, bComp, fComp, _or, _and] = map binary ops
opsFuncs = M.fromList [("+", plus), ("*", times), ("-", minus)
                      , ("/", divide), ("^", expon), ("%", mod)
                      , ("<", lt), (">", gt), ("<=", leq), (">=", geq)
                      , ("==", eq), ("!=", neq), ("|>", fAp), ("$", bAp)
                      , ("~>", fComp), ("<~", bComp)]
(one, two, three) = (Number 1, Number 2, Number 3)
(true, false) = (Constructor "True", Constructor "False")

binOpsTests = [test (T.unpack op) | op <- ops] where
  test op = Test ("can parse `" <> T.pack op <> "'")
                 ("1 " <> op <> " 2")
                 (expr $ binary (T.pack op) one two)

expressionTests = TestGroup "Expressions"
  [
    test "integer" "1" one
  , test "integer 2" "129348" (Number 129348)
  , test "with dot" "1.23" (Number 1.23)
  , ShouldError "with dot, no trailing digits" "10."
  , TestGroup "variables" [
      test "basic variable" "foo" foo
    , test "variable with underscores" "foo_bar" (Var "foo_bar")
    , test "variable with dashes" "foo-bar" (Var "foo-bar")
    , ShouldError "ends with a dash" "foo-"
    , test "variable with primes" "foo'bar" (Var "foo'bar")
    , test "variable with primes and others"
           "foo'oo''" (Var "foo'oo''")
    , test "variable with dashes and others"
           "foo-oo--o" (Var "foo-oo--o")
    , test "variable with bang" "foo!" (Var "foo!")
    , test "variable with bangs" "foo!!" (Var "foo!!")
    , test "variable with bangs and things after it"  "foo!oo"
           (Apply (Var "foo!") (Var "oo"))
    , test "constructor" "Foo" (Constructor "Foo")
    , test "constructor with primes" "Foo'" (Constructor "Foo'")
  ]
  , TestGroup "binary operators" ([
      test "binary" "1 + 2" (plus one two)
    , test "binary 2" "1 + foo" (plus one foo)
    , test "observes precedence rules 1"
            "foo + bar * baz"
            (plus foo (times bar baz))
    , test "observes precedence rules 2"
            "foo * bar + baz"
            (plus (times foo bar) baz)
    , test "observes associativity rules 1"
            "foo - bar - baz"
            (minus (minus foo bar) baz)
    , test "observes associativity rules 2"
            "foo ^ bar ^ baz"
            (expon foo (expon bar baz))
    , test "observes associativity rules 3"
            "foo || bar || baz"
            (_or foo (_or bar baz))
    , test "observes associativity rules 4"
            "foo && bar && baz"
            (_and foo (_and bar baz))
    , test "infix on its own" "_+_" (Var "+")
    , test "alternate infix syntax"
           "_+_ (foo, bar)" (plus foo bar)
    , test "prefix on its own" "+_" (Var "+_")
    , test "postfix on its own" "_+" (Var "_+")
  ] ++ binOpsTests)
  , TestGroup "unary operators" [
      test "prefix operator"
           "-1"
           (Apply (Var "-_") one)
    , test "prefix operator 2"
           "++foo"
           (Apply (Var "++_") foo)
    , test "postfix operator"
           "3!"
           (Apply (Var "_!") three)
    , test "prefix operator in argument"
           "foo (++bar)"
           (Apply foo (Apply (Var "++_") bar))
    , test "postfix operator in argument"
           "foo (bar++)"
           (Apply foo (Apply (Var "_++") bar))
    , test "postfix operator after an application"
           "foo bar++"
           (Apply (Var "_++") $ Apply foo bar)
    , test "mixing postfix and infix operators"
           "foo + bar ++"
           (Apply (Var "_++") $ plus foo bar)
    , test "mixing postfix and infix operators 2"
           "foo + (bar++)"
           (plus foo $ Apply (Var "_++") bar)
    , test "mixing prefix and infix operators"
           "++foo + bar"
           (Apply (Var "++_") $ plus foo bar)
    , test "high precedence prefix"
           "++_ foo" (Apply (Var "++_") foo)
    , test "high precedence prefix 2"
           "++_ foo bar" (Apply (Apply (Var "++_") foo) bar)
  ]
  , test "apply" "foo bar" (Apply foo bar)
  , test "apply associates to the left"
          "foo bar baz"
          (Apply (Apply foo bar) baz)
  , test "no spaces still has apply" "2foo" (Apply two foo)
  , TestGroup "strings" [
      test "basic" "\"hello\"" (String "hello")
    , test "can escape quotes" "\"he said \\\"hello\\\" to me\""
           (String "he said \"hello\" to me")
    , test "can escape newlines" "\"foo\\nbar\""
           (String "foo\nbar")
  ]
  , TestGroup "dot" [
      test "dot" "foo.bar" (Dot foo bar)
    , test "dot associates left"
           "foo.bar.baz"
           (Dot (Dot foo bar) baz)
    , test "dot is higher precedence than apply"
           "foo baz.bar"
           (Apply foo (Dot baz bar))
    , test "dot is higher precedence than apply 2"
           "foo.baz bar"
           (Apply (Dot foo baz) bar)
    , test "dots work on numbers" "1 . foo" (Dot one foo)
    , test "dots work on numbers 2" "1.foo" (Dot one foo)
    , test "dots work on decimals" "2.34 . foo" (Dot (Number 2.34) foo)
    , test "dots work on decimals" "2.34.foo" (Dot (Number 2.34) foo)
    ]
  , TestGroup "Tuples" [
      test "tuples" "(foo, bar)" (tuple [foo, bar])
    , test "tuples 2" "(foo, bar, baz)" (tuple [foo, bar, baz])
    , test "empty tuple" "()" (tuple [])
    , test "tuple of 1 is not a tuple" "(foo)" foo
    , test "nested tuples" "(foo, (bar, baz))"
            (tuple [foo, tuple [bar, baz]])
    , test "nested tuples 2" "((bar, baz, qux), foo)"
            (tuple [tuple [bar, baz, qux], foo])
    , test "tuples with expressions inside" "(foo + 1, bar baz)"
            (tuple [plus foo one, Apply bar baz])
    , test "tuples as arguments" "foo(bar, baz)"
            (Apply foo $ tuple [bar, baz])
    , test "tuples as arguments 2" "foo()bar"
            (Apply (Apply foo $ tuple []) bar)
    ]
  ]
  where test i r e = Test i r (expr e)

arrayTests = TestGroup "Literals"
  [
    TestGroup "array literals" [
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
  , TestGroup "array ranges" [
      Test "make array ranges" "[foo..bar]" (rangeS foo bar)
    , Test "nested with array literals" "[foo..[bar, baz]]"
           (rangeS foo (arrayE [bar, baz]))
    ]
  , TestGroup "lists" [
      Test "basics" "[l foo, bar]" (listS [foo, bar])
  ]
  , TestGroup "lists" [
      Test "basics" "{s foo, bar}" (setS [foo, bar])
  ], TestGroup "dicts" [
      Test "basics" "{d foo => bar, baz => qux}" (dictS [(foo, bar), (baz, qux)])
  ]
  , TestGroup "array dereference" [
      Test "make array reference" "foo[: bar]" [DeRef foo bar]
    , Test "can contain arbitrary expressions"
           "foo [: bar + baz qux]"
           [DeRef foo (plus bar (Apply baz qux))]
    , Test "should have higher precedence than application"
           "foo bar[:baz]" [Apply foo $ DeRef bar baz]
    , Test "should have higher precedence than application 2"
           "(foo bar)[:baz]" [DeRef (Apply foo bar) baz]
    , Test "should have lower precedence than dots"
           "foo.bar[:baz]" [DeRef (Dot foo bar) baz]
    , Test "should have lower precedence than dots 2"
           "foo.(bar[:baz])" [Dot foo (DeRef bar baz)]
    , Test "should associate to the left"
           "foo[:bar][:baz]" [DeRef (DeRef foo bar) baz]
    ]
  ]

typingTests = TestGroup "Typed expressions"
  [
    test "typing an identifier" "foo: Foo" fooT
  , test "typing an identifier with a variable type" "foo: a" a
  , test "typing with 2nd order type" "foo: Maybe Foo" (maybeT fooT)
  , test "typing with 2nd order type using a variable"
         "foo: Maybe a" (maybeT a)
  , test "typing with type tuple" "foo: (Foo, Bar)" (tTuple [fooT, barT])
  , Test "a typed tuple" "(foo: Foo, bar: Bar)"
         [tuple [Typed foo fooT, Typed bar barT]]
  , test "a function" "foo: a -> b" (a ==> b)
  , test "a function 2" "foo: Num -> b" (numT ==> b)
  , test "a function with a tuple" "foo: (Num, Str) -> b"
        (tTuple [numT, strT] ==> b)
  , test "a function 3" "foo: Num -> b -> c" (numT ==> b ==> c)
  , test "a multifunc" "foo: {m Foo -> Bar}" (multi1 fooT barT)
  , test "a multifunc with more than one"
         "foo: {m Foo -> Bar, Baz -> Qux}"
         (multi [(fooT, barT), (bazT, quxT)])
  , test "a multifunc containing functions"
         "foo: {m (Foo -> Bar) -> Baz}"
         (multi1 (fooT ==> barT) bazT)
  , test "a multifunc containing variables"
         "foo: {m (a -> b, a) -> b, Num -> Num}"
         (multi [((tTuple [(a ==> b), a]), b), (numT, numT)])
  , test "a multifunc containing vectors"
         "foo: {m a -> [b], [Num] -> Num}"
         (multi [(a, arrayOf b), (arrayOf numT, numT)])
  , collection "vector" "[" "]" arrayOf
  , collection "list" "[!" "]" listOf
  , collection "set" "{" "}" setOf
  , collection "map1" "{" " => Num}" (\t -> mapOf (t, numT))
  , collection "map2" "{Num =>" "}" (\t -> mapOf (numT, t))
  ]
  where multi = TMultiFunc . M.fromList
        multi1 a b = multi [(a, b)]
        [a, b, c] = map TVar ["a", "b", "c"]
        test name input typ = Test name input [Typed foo typ]
        collection name open close f = TestGroup name tests where
          encl s = "foo: " ++ open ++ s ++ close
          tests = [ test "of variable" (encl "a") (f a)
                  , test "of number" (encl "Num")  (f numT)
                  , test "of function" (encl "Num -> Str") (f (numT ==> strT))
                  , test "of function 2" (encl "[Num] -> {Str}")
                      (f (arrayOf numT ==> setOf strT))
                  , test "of lists" (encl "[!Num]") (f (listOf numT))
                  , test "of vectors" (encl "[Num]") (f (arrayOf numT))
                  , test "of maps" (encl "{a=>b}") (f (mapOf (a, b)))]

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
    , Test "should be able to be used in a binary operator"
           "foo $ bar: Bar => bar + 2"
           (expr $ Apply (Var "$")
                 $ tuple [foo, (Lambda (Typed bar barT)
                               (plus bar two))])
    , Test "should grab as much as we can"
           "bar: Bar => bar + 2 $ foo"
           (eLambda (Typed bar barT) (bAp (plus bar two) foo))
    , Test "should be able to string alternatives"
           "1 => 2 | 2 => 3"
           (eLambdas [(one, two), (two, three)])
    , Test "should be able to handle complex lambdas"
           "(1 => 2 | 2 => 3 | foo => bar baz) 3"
           [Apply (lambdas [(one, two), (two, three), (foo, Apply bar baz)]) three]
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
           [Define "foo" $ Lambda (Typed bar (TVar "a"))
                         $ Lambda (Typed baz (TVar "b"))
                         $ Apply bar baz]
    , Test "should make a function definition with a symbol"
           "(bar: a) <*> (baz: b) = bar baz"
           [Define "<*>" $ Lambda (tuple [ Typed bar (TVar "a")
                                         , Typed baz (TVar "b")])
                         $ Apply bar baz]
    , Test "should make a prefix function definition"
           "!(foo: Bool) = not foo"
           [Define "!_" $ Lambda (Typed foo boolT) (Apply (Var "not") foo)]
    , Test "should make a postfix function definition"
           "(foo: Num)! = fact foo"
           [Define "_!" $ Lambda (Typed foo numT) (Apply (Var "fact") foo)]
    ]
  ]
  where tup1 = tuple [Typed foo fooT, Typed bar barT]
        eLambda arg body = expr $ Lambda arg body
        lambdas abds = Lambdas abds
        eLambdas = expr . lambdas

assignmentTests = TestGroup "Assignments"
  [
    Test "can make definitions" "foo = bar" [Define "foo" bar]
  , Test "can make assignments" "foo := bar" [Assign foo bar]
  , Test "can make complex definitions" "foo = bar + baz"
         [Define "foo" $ bar `plus` baz]
  , Test "can make complex assignments" "foo bar := baz * qux foo"
         [Assign (Apply foo bar) $ baz `times` Apply qux foo]
  ]

objectTests = TestGroup "Object declarations" [
    test1 "can declare a basic object" "object Foo = {Foo}" obj
  , test1 "can declare an object with multiple constructors"
          "object Foo = {Foo; Bar}" (obj {objConstrs = [fooCr, barCr]})
  , test1 "can declare constructors with args"
          "object Foo = {Foo bar}"
          (obj {objConstrs = [fooCr {constrArgs = [bar]}]})
  , test1 "can declare multiple constructors with args"
          "object Foo = {Foo bar; Bar foo}"
          (obj {objConstrs = [ fooCr {constrArgs = [bar]}
                             , barCr {constrArgs = [foo]}]})
  , test1 "can declare extends"
          "object Foo extends Bar = {Foo}"
          (obj {objExtends = (Just "Bar")})
  , test1 "can declare extends"
          "object Foo extends Bar = {Foo}"
          (obj {objExtends = (Just "Bar")})
  , test1 "can declare extends with constructors"
          "object Foo extends Bar = {Foo bar; Bar foo}"
          (obj {objConstrs = [ fooCr {constrArgs = [bar]}
                             , barCr {constrArgs = [foo]}]
               , objExtends = (Just "Bar")})
  , test1 "can declare extends with constructors that extend"
          "object Foo extends Bar = {Foo bar extends Bar; Bar extends Foo}"
          (obj {objConstrs = [ fooCr {constrArgs = [bar]
                                    , constrExtends = Just barC}
                             , barCr {constrExtends = Just fooC}]
               , objExtends = (Just "Bar")})
  , test1 "constructors with logic"
          ("object Foo extends Bar = {" <>
           "Foo bar do baz = 1; " <>
           "Bar {qux = foo bar; print \"hello\"}}")
          (obj {objConstrs = [ fooCr {constrArgs = [bar]
                                    , constrLogic = Just expr1}
                             , barCr {constrLogic = Just expr2}]
               , objExtends = (Just "Bar")})
  , test1 "generics" "object Foo a = {Bar; Foo (bar: a)}"
          (obj {objVars = ["a"],
                objConstrs = [barCr, fooCr {constrArgs = [Typed bar a]}]})
  , test1 "with attributes"
          "object Foo a = {Bar; Foo (bar: a) with foo: Num; bar: Str}"
          (obj { objVars = ["a"]
               , objConstrs = [barCr, fooCr {constrArgs = [Typed bar a]}]
               , objAttrs = [Typed foo numT, Typed bar strT]})
  ] where
      test1 desc input ex = Test desc input (expr $ ObjDec ex)
      obj = defObj { objName = "Foo", objConstrs = [fooCr]}
      fooCr = defConstr {constrName = "Foo"}
      barCr = defConstr {constrName = "Bar"}
      expr1 = Define "baz" one
      expr2 = Block [Define "qux" (Apply foo bar), print_ (String "hello")]
      a = TVar "a"

rassocTests = TestGroup "Right-associative functions"
  [
    Test "rassoc doesn't matter when single expr"
         "rassoc foo; foo"
         [foo]
  , Test "rassoc doesn't matter when one arg"
         "rassoc foo; foo bar"
         [Apply foo bar]
  , Test "rassoc doesn't matter when it is the arg"
         "rassoc bar; foo bar"
         [Apply foo bar]
  , Test "rassoc doesn't matter when it is the arg 2"
         "rassoc bar; foo bar baz"
         [Apply (Apply foo bar) baz]
  , Test "rassoc matters when 2 args"
         "rassoc foo; foo bar baz"
         [Apply foo (Apply bar baz)]
  , Test "rassoc works with binary functions"
         "rassoc foo; foo bar + baz"
         [Apply foo (plus bar baz)]
  , Test "rassoc works with binary functions 2"
         "rassoc foo; foo bar baz + qux"
         [Apply foo (plus (Apply bar baz) qux)]
  , Test "rassoc works in parentheses"
         "rassoc bar; foo (bar baz qux)"
         [Apply foo (Apply bar (Apply baz qux))]
  , Test "nested rassoc works"
         "rassoc foo, bar; foo bar baz"
         [Apply foo (Apply bar baz)]
  , Test "nested rassoc works 2"
         "rassoc foo, bar; foo bar baz qux"
         [Apply foo (Apply bar (Apply baz qux))]
  , Test "nested rassoc works 3"
         "rassoc foo, bar; foo bar baz + qux"
         [Apply foo (Apply bar (plus baz qux))]
  , Test "print is rassoc by default"
         "print 1 + 2; print foo bar"
         [print_ (plus one two), print_ (Apply foo bar)]
  , Test "assert is rassoc by default"
         "assert 1 != 2; assert foo bar > 3"
         [assert (neq one two), assert (gt (Apply foo bar) three)]
  , Test "assert is rassoc by default"
         "rassoc foo; assert (foo bar baz) > 3"
         [assert (gt (Apply foo (Apply bar baz)) three)]
  , Test "we can still pass print and assert as args"
         "map print foo; map assert bar"
         [ Apply (Apply (Var "map") (Var "print")) foo
         , Apply (Apply (Var "map") (Var "assert")) bar]
  -- Should test that we can unrassoc something, and that this
  -- happens automatically when shadowing a definition
  ]

blockTests = TestGroup "Blocks"
  [
    SkipTest "separate expressions into blocks by newline"
          "foo\nbar\nbaz qux"
          [foo, bar, Apply baz qux]
  , Test "separate expressions into blocks by semicolon"
         "foo;bar;baz qux"
         [foo, bar, Apply baz qux]
  , Test "after" "foo after bar" [foo `After` bar]
  , Test "after with block" "foo after {bar; baz}"
         [foo `After` Block [bar, baz]]
  , Test "before" "foo before bar" [foo `Before` bar]
  , Test "before with block" "foo before {bar; baz}"
         [foo `Before` Block [bar, baz]]
  , SkipTest "after with block 2" "{foo; bar} after {bar; baz}"
         [Block [foo, bar] `After` Block [bar, baz]]
  , SkipTest "before with block 2" "{foo; bar} before {bar; baz}"
         [Block [foo, bar] `Before` Block [bar, baz]]
  ]

flowTests = TestGroup "Program flow"
  [
    Test "return" "return" [Return unit]
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
  , Test "nested" "if True then 1 else if False then 2 else 3"
    [If true one (If false two three)]
  , Test "nested 2" "if True then if False then 1 else 2 else 3"
    [If true (If false one two) three]
  , Test "nested 3" "if if True then False else True then 1 else 2"
    [If (If true false true) one two]
  ]

caseTests = TestGroup "Case statements"
  [
    Test "basic" "case 1 of 2 => 3" [Case one [(two, three)]]
  , Test "multiple" "case 1 of 2 => 3 | 3 => 1"
         [Case one [(two, three), (three, one)]]
  , Test "with variable" "case foo of 2 => bar 3 | baz 3 => 1"
         [Case foo [(two, Apply bar three), (Apply baz three, one)]]
  , Test "separated by semicolon"
         "case foo of 2 => bar 3 | baz 3 => 1; case 1 of 2 => 3"
         [Case foo [(two, Apply bar three), (Apply baz three, one)]
         , Case one [(two, three)]]
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
                        , caseTests
                        , flowTests
                        , rassocTests
                        , objectTests
                        ]

main = doTests
