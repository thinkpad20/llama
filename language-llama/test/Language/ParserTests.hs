{-# LANGUAGE OverloadedStrings #-}
module ParserTests (allTests, main) where

import qualified Data.Map as M
import qualified Data.Text as T
import Prelude hiding (mod)
import Data.Text (Text)

import Common
import Tests
import AST
import TypeLib
import Parser (grab)

skipTestB :: Name -> input -> Block -> Test input Expr
skipTestB desc i exprs = SkipTest desc i (Block exprs)
foo, bar, baz, qux, fooC, barC, one, two, three :: Expr
(foo, bar, baz, qux) = (Var "foo", Var "bar", Var "baz", Var "qux")
[fooC, barC] = map Constructor ["Foo", "Bar"]
(one, two, three) = (Number 1, Number 2, Number 3)
fooT, barT, bazT, quxT :: Type
(fooT, barT, bazT, quxT) = ( tConst "Foo", tConst "Bar"
                           , tConst "Baz", tConst "Qux")
print_, assert :: Expr -> Expr
print_ = Apply (Var "print")
assert = Apply (Var "assert")
listE, setE, arrayE :: [Expr] -> Expr
listE exprs = Literal $ ListLiteral exprs
setE exprs = Literal $ SetLiteral exprs
arrayE exprs = Literal $ ArrayLiteral exprs
dictE :: [(Expr, Expr)] -> Expr
dictE exprs = Literal $ DictLiteral exprs
rangeE :: Expr -> Expr -> Expr
rangeE start stop = ArrayRange start stop ! Literal
ops :: [Text]
ops = [ "+", "*", "-", "^", "<", ">", "<="
      , ">=", "==", "!=", "$", "|>", "<~", "~>", "||", "&&"]
plus, times, minus, expon, gt, neq, bAp, _or, _and :: Expr -> Expr -> Expr
(plus, times, minus, expon) = (binary "+", binary "*", binary "-", binary "^")
(gt, neq, bAp) = (binary ">", binary "!=", binary "$")
(_or, _and) = (binary "||", binary "&&")

binOpsTests :: [Test String Expr]
binOpsTests = [test (T.unpack op) | op <- ops] where
  test op = Test ("can parse `" <> T.pack op <> "'")
                 ("1 " <> op <> " 2")
                 (binary (T.pack op) one two)

expressionTests :: Test String Expr
expressionTests = TestGroup "Expressions"
  [
    test "integer" "1" one
  , test "integer 2" "129348" (Number 129348)
  , test "with dot" "1.23" (Number 1.23)
  , ShouldError "with dot, no trailing digits" "10."
  , TestGroup "variables" [
      test "basic variable" "foo" foo
    , test "variable with underscores" "foo_bar" (Var "foo_bar")
    , test "variable starting with underscores" "_foo" (Var "_foo")
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
    --, test "infix on its own" "(+)" (Var "+")
    --, test "alternate infix syntax"
    --       "(+) (foo, bar)" (plus foo bar)
  ] ++ binOpsTests)
  , SkipTestGroup "prefixes" [
      test "prefix operator" "-1" (Prefix "-" one)
    , test "prefix operator 2" "++foo" (Prefix "++" foo)
    , Test "prefix after a statement" "1; +2" $ Block [one, Prefix "+" two]
    , ShouldError "prefix operator in argument" "foo (++bar)"
    , ShouldError "mixing prefix and infix operators" "++foo + bar"
    , ShouldError "high precedence prefix" "++_ foo"
    , ShouldError "high precedence prefix 2" "++_ foo bar"
  ]
  , test "apply" "foo bar" (Apply foo bar)
  , test "apply associates to the left"
          "foo bar baz"
          (Apply (Apply foo bar) baz)
  , test "no spaces still has apply" "2foo" (Apply two foo)
  , TestGroup "strings" [
      test "basic" "\"hello\"" (InString "hello")
    , test "can escape quotes" "\"he said \\\"hello\\\" to me\""
           (InString "he said \"hello\" to me")
    , test "can escape newlines" "\"foo\\nbar\""
           (InString "foo\nbar")
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
    , test "tuple with eq kwarg" "(foo, @bar=baz)"
            (Tuple [foo] [("bar", Left baz)])
    , test "tuple with typed kwarg" "(foo, @bar:baz)"
            (Tuple [foo] [("bar", Right (TVar "baz"))])
    , test "tuple with both kwargs" "(foo, @bar=baz, @qux: Num)"
            (Tuple [foo] [("bar", Left baz), ("qux", Right numT)])
    , test "tuple with only kwarg" "(@foo=bar)" (Tuple [] [("foo", Left bar)])
    , ShouldError "kwargs not at the end" "(@foo=bar, baz)"
    ]
  ]
  where test i r e = Test i r e

arrayTests :: Test String Expr
arrayTests = TestGroup "Literals"
  [
    TestGroup "array literals" [
      Test "make array literals" "[foo, bar, baz]"
           (arrayE [foo, bar, baz])
    , Test "empty array" "[]" (arrayE [])
    , Test "with complex expressions" "[foo * 1 + bar, baz (qux foo.bar)]"
           (arrayE [ plus (times foo one) bar
                   , Apply baz (Apply qux (Dot foo bar))])
    , Test "nested" "[foo, [bar, baz]]" (arrayE [foo, arrayE [bar, baz]])
    , Test "use as arguments" "foo [1, bar, baz]"
           (Apply foo $ arrayE [one, bar, baz])
    ]
  , TestGroup "array ranges" [
      Test "make array ranges" "[foo..bar]" (rangeE foo bar)
    , Test "nested with array literals" "[foo..[bar, baz]]"
           (rangeE foo (arrayE [bar, baz]))
    ]
  , TestGroup "lists" [
      Test "basics" "[l foo, bar]" (listE [foo, bar])
  ]
  , TestGroup "lists" [
      Test "basics" "{s foo, bar}" (setE [foo, bar])
  ], TestGroup "dicts" [
      Test "basics" "{d foo => bar, baz => qux}" (dictE [(foo, bar), (baz, qux)])
  ]
  , TestGroup "array dereference" [
      Test "make array reference" "foo[: bar]" (DeRef foo bar)
    , Test "can contain arbitrary expressions"
           "foo [: bar + baz qux]"
           (DeRef foo (plus bar (Apply baz qux)))
    , Test "should have higher precedence than application"
           "foo bar[:baz]" (Apply foo $ DeRef bar baz)
    , Test "should have higher precedence than application 2"
           "(foo bar)[:baz]" (DeRef (Apply foo bar) baz)
    , Test "should have lower precedence than dots"
           "foo.bar[:baz]" (DeRef (Dot foo bar) baz)
    , Test "should have lower precedence than dots 2"
           "foo.(bar[:baz])" (Dot foo (DeRef bar baz))
    , Test "should associate to the left"
           "foo[:bar][:baz]" (DeRef (DeRef foo bar) baz)
    ]
  ]

typingTests :: Test String Expr
typingTests = TestGroup "Typed expressions"
  [
    test "typing an identifier" "foo: Foo" fooT
  , test "typing an identifier with a variable type" "foo: a" a
  , test "typing with 2nd order type" "foo: Maybe Foo" (maybeT fooT)
  , test "typing with 2nd order type using a variable"
         "foo: Maybe a" (maybeT a)
  , test "typing with type tuple" "foo: (Foo, Bar)" (tTuple [fooT, barT])
  , Test "a typed tuple" "(foo: Foo, bar: Bar)"
         (tuple [Typed foo fooT, Typed bar barT])
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
        multi1 x y = multi [(x, y)]
        [a, b, c] = map TVar ["a", "b", "c"]
        test name input typ = Test name input (Typed foo typ)
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

functionTests :: Test String Expr
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
           (PatternDef foo (Lambda bar (plus bar one)))
    , Test "should be able to be used in assignments"
           "foo := bar: Baz => qux bar"
           (Assign foo (Lambda (Typed bar bazT) (Apply qux bar)))
    , Test "should be able to be used in a binary operator"
           "foo $ bar: Bar => bar + 2"
           (Apply (Apply (Var "$") foo) $ (Lambda (Typed bar barT) (plus bar two)))
    , Test "should grab as much as we can"
           "bar: Bar => bar + 2 $ foo"
           (eLambda (Typed bar barT) (bAp (plus bar two) foo))
    , Test "should be able to string alternatives"
           "1 => 2 | 2 => 3"
           (lambdas [(one, two), (two, three)])
    , Test "should be able to handle complex lambdas"
           "(1 => 2 | 2 => 3 | foo => bar baz) 3"
           (Apply (lambdas [(one, two), (two, three), (foo, Apply bar baz)]) three)
    ]
  , TestGroup "Patterns & defined functions" [
      Test "should make a function definition"
           "foo (bar: Bar) = bar"
           (PatternDef (Apply foo (Typed bar barT)) bar)
    , Test "should make a function definition with multiple args"
           "foo(bar: Bar) (baz: Baz) = bar + baz"
           (PatternDef (Apply (Apply foo (Typed bar barT)) (Typed baz bazT))
                        (plus bar baz))
    , Test "should make a function definition with polymorphic args"
           "foo(bar: a) (baz: b) = bar baz"
           (PatternDef (Apply (Apply foo (Typed bar $ TVar "a")) (Typed baz $ TVar "b"))
                       (Apply bar baz))
    , Test "should make a function definition with a symbol"
           "(bar: a) <*> (baz: b) = bar baz"
           (PatternDef (binary "<*>" (Typed bar (TVar "a"))
                                     (Typed baz (TVar "b"))) $ Apply bar baz)
    , Test "should sugar tuple assignment" "foo, bar = (1, 2)"
           (PatternDef (tuple [foo, bar]) (tuple [one, two]))
    ]
  , TestGroup "LambdaDots" [
      Test "basic" ".foo" (LambdaDot foo)
    , Test "with other args" ".foo bar" (Apply (LambdaDot foo) bar)
    , Test "in assignment" "foo = .bar" (PatternDef foo $ LambdaDot bar)
    , Test "in assignment with other exprs" "foo = .bar baz"
           (PatternDef foo $ Apply (LambdaDot bar) baz)
  ]
  ]
  where tup1 = tuple [Typed foo fooT, Typed bar barT]
        eLambda arg body = Lambda arg body
        lambdas abds = Lambdas abds

assignmentTests :: Test String Expr
assignmentTests = TestGroup "Assignments"
  [
    Test "can make definitions" "foo = bar" (PatternDef foo bar)
  , Test "can make definitions with underscores"
         "_foo = bar" (PatternDef (Var "_foo") bar)
  , Test "can make assignments" "foo := bar" (Assign foo bar)
  , Test "can make complex definitions" "foo = bar + baz"
         (PatternDef foo $ bar `plus` baz)
  , Test "can make complex assignments" "foo bar := baz * qux foo"
         (Assign (Apply foo bar) $ baz `times` Apply qux foo)
  ]

objectTests :: Test String Expr
objectTests = TestGroup "Object declarations" [
    test1 "can declare an object without any definition" "object Foo" obj'
  , test1 "can declare a basic object" "object Foo = Foo" obj
  , test1 "can declare an object with multiple constructors"
          "object Foo = Foo | Bar" (obj {objConstrs = [fooCr, barCr]})
  , test1 "can declare constructors with args"
          "object Foo = Foo bar"
          (obj {objConstrs = [fooCr {constrArgs = [bar]}]})
  , test1 "can declare a basic object with named & typed arguments"
          "object Foo = Foo (foo: Num) (bar: Str)"
          (obj {objConstrs = [fooCr {constrArgs = [Typed foo numT
                                                 , Typed bar strT]}]})
  , test1 "can declare a polymorphic object"
          "object Foo a = Foo (foo: Num) (bar: a)"
          (obj {objConstrs = [fooCr {constrArgs = [Typed foo numT
                                                 , Typed bar (TVar "a")]}]
               , objVars = ["a"]})
  , test1 "can declare multiple constructors with args"
          "object Foo = Foo bar | Bar foo"
          (obj {objConstrs = [ fooCr {constrArgs = [bar]}
                             , barCr {constrArgs = [foo]}]})
  , test1 "can declare extends"
          "object Foo <: Bar = Foo"
          (obj {objExtends = (Just "Bar")})
  , test1 "can declare extends without constructors"
          "object Foo <: Bar"
          (obj' {objExtends = (Just "Bar")})
  , test1 "can declare extends with constructors"
          "object Foo <: Bar = Foo bar | Bar foo"
          (obj {objConstrs = [ fooCr {constrArgs = [bar]}
                             , barCr {constrArgs = [foo]}]
               , objExtends = (Just "Bar")})
  , test1 "can declare extends with constructors that extend"
          "object Foo <: Bar = Foo bar <: Bar | Bar <: Foo"
          (obj {objConstrs = [ fooCr {constrArgs = [bar]
                                    , constrExtends = Just barC}
                             , barCr {constrExtends = Just fooC}]
               , objExtends = (Just "Bar")})
  , test1 "can declare extends with an arbitrary expression"
          "object Foo <: Bar = Foo bar <: bar"
          (obj {objExtends = Just "Bar",
                objConstrs = [fooCr { constrArgs = [bar]
                                    , constrExtends = Just bar}]})
  , test1 "can declare extends with an arbitrary expression and a with statement"
          "object Foo <: Bar = Foo bar <: bar with baz: Num"
          (obj {objExtends = Just "Bar",
                objConstrs = [fooCr { constrArgs = [bar]
                                    , constrExtends = Just bar}],
                objAttrs = [Typed baz numT]})
  , test1 "extended expression can have its own with statement"
          "object Foo <: Bar = Foo bar <: bar with baz = 1"
          (obj {objExtends = Just "Bar",
                objConstrs = [fooCr { constrArgs = [bar]
                                    , constrExtends = Just (With bar [("baz", one)])}]})
  , test1 "extended expression can have its own with statement, and the object has withs as well"
          "object Foo <: Bar = Foo bar <: bar with baz = 1 with qux: Num"
          (obj {objExtends = Just "Bar",
                objConstrs = [fooCr { constrArgs = [bar]
                                    , constrExtends = Just (With bar [("baz", one)])}],
                objAttrs = [Typed qux numT]})
  , test1 "constructors with logic"
          ("object Foo <: Bar = " <>
           "Foo bar do baz = 1 | " <>
           "Bar {qux = foo bar; print \"hello\"}")
          (obj {objConstrs = [ fooCr {constrArgs = [bar]
                                    , constrLogic = Just expr1}
                             , barCr {constrLogic = Just expr2}]
               , objExtends = (Just "Bar")})
  , test1 "generics" "object Foo a = Bar | Foo (bar: a)"
          (obj {objVars = ["a"],
                objConstrs = [barCr, fooCr {constrArgs = [Typed bar a]}]})
  , test1 "with single attribute"
          "object Foo a = Bar | Foo (bar: a) with foo: Num"
          (obj { objVars = ["a"]
               , objConstrs = [barCr, fooCr {constrArgs = [Typed bar a]}]
               , objAttrs = [Typed foo numT]})
  , test1 "with attributes"
          "object Foo a = Bar | Foo (bar: a) with {foo: Num; bar: Str}"
          (obj { objVars = ["a"]
               , objConstrs = [barCr, fooCr {constrArgs = [Typed bar a]}]
               , objAttrs = [Typed foo numT, Typed bar strT]})
  , test1 "object with only attributes" "object Foo with bar: Bar"
          (obj' {objAttrs=[Typed bar barT]})
  ] where
      test1 desc input ex = Test desc input (ObjDec ex)
      obj = defObj { objName = "Foo", objConstrs = [fooCr]}
      obj' = obj {objConstrs=[]}
      fooCr = defConstr {constrName = "Foo"}
      barCr = defConstr {constrName = "Bar"}
      expr1 = PatternDef baz one
      expr2 = Block [PatternDef qux (Apply foo bar), print_ (InString "hello")]
      a = TVar "a"

--withTests :: Test String Expr
--withTests

rassocTests :: Test String Expr
rassocTests = TestGroup "Right-associative functions"
  [
    Test "rassoc doesn't matter when single expr"
         "rassoc foo; foo"
         foo
  , Test "rassoc doesn't matter when one arg"
         "rassoc foo; foo bar"
         (Apply foo bar)
  , Test "rassoc doesn't matter when it is the arg"
         "rassoc bar; foo bar"
         (Apply foo bar)
  , Test "rassoc doesn't matter when it is the arg 2"
         "rassoc bar; foo bar baz"
         (Apply (Apply foo bar) baz)
  , Test "rassoc matters when 2 args"
         "rassoc foo; foo bar baz"
         (Apply foo (Apply bar baz))
  , Test "rassoc works with binary functions"
         "rassoc foo; foo bar + baz"
         (Apply foo (plus bar baz))
  , Test "rassoc works with binary functions 2"
         "rassoc foo; foo bar baz + qux"
         (Apply foo (plus (Apply bar baz) qux))
  , Test "rassoc works in parentheses"
         "rassoc bar; foo (bar baz qux)"
         (Apply foo (Apply bar (Apply baz qux)))
  , Test "nested rassoc works"
         "rassoc foo, bar; foo bar baz"
         (Apply foo (Apply bar baz))
  , Test "nested rassoc works 2"
         "rassoc foo, bar; foo bar baz qux"
         (Apply foo (Apply bar (Apply baz qux)))
  , Test "nested rassoc works 3"
         "rassoc foo, bar; foo bar baz + qux"
         (Apply foo (Apply bar (plus baz qux)))
  , Test "print is rassoc by default"
         "print 1 + 2; print foo bar"
         (Block [print_ (plus one two), print_ (Apply foo bar)])
  , Test "assert is rassoc by default"
         "assert 1 != 2; assert foo bar > 3"
         (Block [assert (neq one two), assert (gt (Apply foo bar) three)])
  , Test "assert is rassoc by default"
         "rassoc foo; assert (foo bar baz) > 3"
         (assert (gt (Apply foo (Apply bar baz)) three))
  , Test "we can still pass print and assert as args"
         "map print foo; map assert bar"
         (Block [ Apply (Apply (Var "map") (Var "print")) foo
                , Apply (Apply (Var "map") (Var "assert")) bar])
  -- Should test that we can unrassoc something, and that this
  -- happens automatically when shadowing a definition
  ]

blockTests :: Test String Expr
blockTests = TestGroup "Blocks"
  [
    skipTest "separate expressions into blocks by newline"
          "foo\nbar\nbaz qux"
          (Block [foo, bar, Apply baz qux])
  , test "separate expressions into blocks by semicolon"
         "foo;bar;baz qux"
         (Block [foo, bar, Apply baz qux])
  , test "after" "foo after bar" (foo `After` bar)
  , test "after with block" "foo after {bar; baz}"
         (foo `After` Block [bar, baz])
  , test "before" "foo before bar" (foo `Before` bar)
  , test "before with block" "foo before {bar; baz}"
         (foo `Before` Block [bar, baz])
  , skipTest "after with block 2" "{foo; bar} after {bar; baz}"
         (Block [foo, bar] `After` Block [bar, baz])
  , skipTest "before with block 2" "{foo; bar} before {bar; baz}"
         (Block [foo, bar] `Before` Block [bar, baz])
  ]
  where test descr i es = Test descr i es
        skipTest descr i es = SkipTest descr i es

flowTests :: Test String Expr
flowTests = TestGroup "Program flow"
  [
    Test "return" "return" (Return unit)
  , Test "return with expr" "return 3" (Return three)
  , Test "return in a series of statements"
         "foo bar; return 3; baz qux"
         (Block [Apply foo bar, Return three, Apply baz qux])
  ]

ifTests :: Test String Expr
ifTests = TestGroup "If statements"
  [
    Test "basic one-line" "if 1 then 2 else 3" (If one two three)
  , Test "basic block" "if 1 { 2 } else { 3 }" (If one two three)
  , Test "basic block" "if 1 { 2 } else { 3 }" (If one two three)
  , Test "basic mixed 1" "if 1 then 2 else { 3 }" (If one two three)
  , Test "basic mixed 2" "if 1 { 2 } else 3" (If one two three)
  , Test "without else" "if 1 { 2 }; if 2 then 3"
    (Block [If' one two, If' two three])
  , Test "without else then with" "if 1 { 2 }; if 2 then 3 else 1"
    (Block [If' one two, If two three one])
  , Test "used in lambda" "n: Num => if n then 2 else 3"
    (Lambda (Typed (Var "n") numT) $ If (Var "n") two three)
  , Test "used in define" "foo (n: Num) = if n then 2 else 3"
    (PatternDef (Apply foo (Typed (Var "n") numT)) $ If (Var "n") two three)
  , Test "nested" "if True then 1 else if False then 2 else 3"
    (If true one (If false two three))
  , Test "nested 2" "if True then if False then 1 else 2 else 3"
    (If true (If false one two) three)
  , Test "nested 3" "if if True then False else True then 1 else 2"
    (If (If true false true) one two)
  ]

caseTests :: Test String Expr
caseTests = TestGroup "Case statements"
  [
    Test "basic" "case 1 of 2 => 3" (MultiCase one [([two], three)])
  , Test "multiple options" "case 1 of 2 => 3 | 3 => 1"
         (MultiCase one [([two], three), ([three], one)])
  , Test "multiple patterns" "case 1 of 1, 2 => 3 | 3 => 1"
         (MultiCase one [([one, two], three), ([three], one)])
  , Test "multiple patterns 2" "case 1 of 1, 2 => 3 | 3, 1, foo bar => 1"
         (MultiCase one [([one, two], three), ([three, one, Apply foo bar], one)])
  , Test "with variable" "case foo of 2 => bar 3 | baz 3 => 1"
         (MultiCase foo [([two], Apply bar three), ([Apply baz three], one)])
  , Test "separated by semicolon"
         "case foo of 2 => bar 3 | baz 3 => 1; case 1 of 2 => 3"
         (Block [MultiCase foo [([two], Apply bar three), ([Apply baz three], one)]
         , MultiCase one [([two], three)]])
  ]

forTests :: Test String Expr
forTests = TestGroup "For statements"
 [
    TestGroup "single statements" [
      Test "for block via 'do'" "for foo; bar; baz do qux;"
           (For foo bar baz qux)
    , skipTestB "for block via 'do' with following expression"
           "for foo; bar; baz do qux\nfoo"
           [For foo bar baz qux, baz]
    , Test "for block via 'do' with following expression, split by semicolon"
           "for foo; bar; baz do qux; foo"
           (Block [For foo bar baz qux, foo])
    ]
  , TestGroup "using curly braces" [
      Test "basic" "for foo; bar; baz {2}"
           (For foo bar baz two)
    , Test "multiple statements"
           "for foo; bar; baz {2; 3}"
           (For foo bar baz $ Block [two, three])
    , skipTestB "complex statements"
           "for foo bar; baz; qux; {bar 2; baz 3}"
           [For (Apply foo bar) baz qux $ Block [Apply bar two, Apply baz three]]
    , Test "followed by other statement, using semicolon"
           "for foo; bar; baz { qux }; foo"
           (Block [For foo bar baz qux, foo])
    , Test "nested for statements 1"
           "for foo; bar; baz { for bar; baz; qux {3}}"
           (For foo bar baz (For bar baz qux three))
    , Test "nested for statements 2"
           "for foo; bar; baz {2; for bar; baz; qux { 3}; baz}; qux"
           (Block [For foo bar baz $ Block [two, For bar baz qux three, baz], qux])
    ]
  ]

forInTests :: Test String Expr
forInTests = TestGroup "For statements" tests where
  tests =
    [
      Test "basic" "for foo in bar { baz }" (ForIn foo bar baz)
    , Test "basic one-line" "for foo in bar do baz" (ForIn foo bar baz)
    , Test "nested via one-line" "for foo in bar do for baz in qux do 1"
      (ForIn foo bar (ForIn baz qux one))
    ]

doTests :: IO TesterState
doTests = runTests grab [ expressionTests
                        , assignmentTests
                        , arrayTests
                        , blockTests
                        , forTests
                        , forInTests
                        , typingTests
                        , functionTests
                        , ifTests
                        , caseTests
                        , flowTests
                        , rassocTests
                        , objectTests
                        ]

allTests :: [TesterState -> IO TesterState]
allTests = [run grab [ expressionTests
                     , assignmentTests
                     , arrayTests
                     , blockTests
                     , forTests
                     , forInTests
                     , typingTests
                     , functionTests
                     , ifTests
                     , caseTests
                     , flowTests
                     , rassocTests
                     , objectTests
                     ]
            ]

main :: IO ()
main = doTests >> return ()
