## Case statement for control flow?

```
foo (n: Num) =
  case n of
    1 => print "The number is one"
         return -3
    2 => print "The number isn't one"
  if n % 2 == 0 then n + 1 else n * 7

foo (n: Num) =
  if n == 1
    print "The number is one"
    return -3
  else if n == 2
    print "The number isn't one"
  if n % 2 == 0 then n + 1 else n * 7
```

Or, we could have `case` for expressions, and `switch` for statements; the
idea being that a case statement must be exhaustive, but a switch statement
needn't be (`case` is analogous to a ternary operator, while `switch` is more
like a series of `if` statements which may or may not be terminated by an
`else`).

```
foo (n: Num, @m: Num) =
  result = case m of Just 1  => 2
                     Just 2  => 14
                     Just n  => 3
                     Nothing => 16
  bar = mut "#{n}. Hey there"
  switch result
    2  => return "You entered one!"
    14 => bar += ", you sexy thing"
    16 => bar := if n % 2 == 0 then " yikes!" else " whew"
  result

print <| foo 1      # 1. Hey there whew
print <| foo 2      # 2. Hey there yikes!
print <| foo (3, 2) # 3. Hey there, you sexy thing
print <| foo (4, 5) # 4. Hey there
print <| foo (0, 1) # You entered one!
```

Further, it suggests that we apply the following syntactic sugar rule:

```
a => b # equivalent to (\a -> b)
a => b | c => d # equivalent to (\ _arg -> case _arg of a => b | c => d)
```

That would maintain the rule we had stating that all alternatives in a lambda
must have the same type. How about multiple dispatch?

```
foo (n: Num) = n + 1
foo (s: Str) = reverse s

print <| foo 2       # 3
print <| foo "hello" # olleh
```

So how do we handle that? One possibility is to use `def` or similar to
indicate that we're defining a function. But we'd rather avoid that if
possible. We should probably simply have separate parsing rules for that...
But still, what does it parse to?

```
# input1.llm
foo (n: Num) = n + 1
foo (s: Str) = reverse s
bar = 3
print (foo bar)

# input2.llm
foo = (n: Num) => n + 1
foo = (s: Str) => reverse s
bar = "hello"
print (foo bar)

# input3.llm
foo = 1 => 2 | 2 => 0 | n: Num => { print "hey"; n + 1 }
bar = 5
print (foo bar)
```

```
ghci> parseFile "input1.llm"
[ Define "foo" (Function (Typed (Var "n") (TConst "Num" [])) [Expr (Apply (Apply (Var "+") (Var "n")) (Number 1))])
, Define "foo" (Function (Typed (Var "s") (TConst "Str" [])) [Expr (Apply (Var "reverse") (Var "s"))])
, Define "bar" (Expr (Number 3))
, Expr (Apply (Var "print") (Apply (Var "foo") (Var "bar"))) ]
ghci> parseFile "input2.llm"
[ Define "foo" (Lambda (Typed (Var "n") (TConst "Num" [])) (Apply (Apply (Var "+") (Var "n")) (Number 1))])
, Define "foo" (Lambda (Typed (Var "s") (TConst "Str" [])) [Expr (Apply (Var "reverse") (Var "s"))])
, Define "bar" (Expr (String "hello"))
, Expr (Apply (Var "print") (Apply (Var "foo") (Var "bar"))) ]
ghci> parseFile "input3.llm"
[ Define "foo" (LambdaAlts [ (Number 1, Number 2)
                           , (Number 2, Number 0)
                           , (Typed (Var "n") (TConst "Num" [])
                             , Apply (Apply (Var "+") (Var "n")) (Number 1))])
, Define "bar" (Number 5)
, Expr (Apply (Var "print") (Apply (Var "foo") (Var "bar"))) ]
```

So we're separating "Functions" from "Lambdas"... is that necessary? The only
practical difference between the two is that lambdas support alternatives and
functions don't. So fuck it, let's not make the distinction.

Also, `LambdaAlts` seems like a pain in the ass. Lambdas with one alternative
are just a special case of LambdaAlts, so let's parse everything as `Lambdas`,
which desugar into a single `Lambda` which maps to a `case` expression.

```
ghci> parseFile "input1.llm"
[ Define "foo" (Lambdas [( Typed (Var "n") (TConst "Num" [])
                         , Apply (Apply (Var "+") (Var "n")) (Number 1))])
, Define "foo" (Lambdas [( Typed (Var "s") (TConst "Str" [])
                         , Apply (Var "reverse") (Var "s"))])
, Define "bar" (Number 3)
, Expr (Apply (Var "print") (Apply (Var "foo") (Var "bar"))) ]
ghci> parseFile "input2.llm"
[ Define "foo" (Lambdas [( Typed (Var "n") (TConst "Num" [])
                         , Apply (Apply (Var "+") (Var "n")) (Number 1))])
, Define "foo" (Lambdas [( Typed (Var "s") (TConst "Str" [])
                         , Apply (Var "reverse") (Var "s"))])
, Define "bar" (String "hello")
, Expr (Apply (Var "print") (Apply (Var "foo") (Var "bar"))) ]
ghci> parseFile "input3.llm"
[ Define "foo" (Lambdas [ (Number 1, Number 2)
                        , (Number 2, Number 0)
                        , ( Typed (Var "n") (TConst "Num" [])
                          , Block [ Expr (Apply (Var "print") (String "hey"))
                                  , Expr (Apply (Apply (Var "+") (Var "n")) (Number 1))])])
, Define "bar" (Number 5)
, Expr (Apply (Var "print") (Apply (Var "foo") (Var "bar"))) ]
```

So, changes to make:

```
data Expr = ...
          | Block Block
          | Lambdas [(Expr, Expr)]
          | Lambda Expr Expr
          | Case Expr [(Expr, Expr)]

data Statement = ...
               | Define Name Expr
               | Switch Expr [(Expr, Block)]

```

Alternatively, we could separate out `SugaredExpr` and `Expression`, with the
former supporting syntactic sugar (possibly along with the parsed position)
and the latter not (perhaps the latter retains the parsing position and some
other data useful for debugging?). For example:

```
data Source = { fileName :: Name
              , lineNumber :: Int
              , columnNumber :: Int
              , sourceText :: String }
data ParsedExpr = PE Source SExpression
data ParsedStmt = PE Source SStatement

type SBlock = [SStatement]
data SStatement = ...
                | DefineVariable Name SExpression
                | DefineFunction Name [(Name, Type)] SBlock
                | DefineBinary Name (Name, Type) (Name, Type) SBlock
                | DefinePrefix Name (Name, Type) SBlock
                | DefinePostfix Name (Name, Type) SBlock

data SExpression = ...
                 | Lambdas [(SExpression, SExpression)]
                 | ...

data Statement = ...
               | Define Name Expression

data Expression = ...
                | Block Block
                | Lambda Expression Expression
                | Case [(Expression, Expression)]

desugarStmt  :: SStatement -> Statement
desugarExpr  :: SExpression -> Expression
desugarBlock :: SBlock -> Block
```

I think this is probably a good idea, if for no other reason that it encourages
the separation of what we *parse* from what we *evaluate*. For example,
optimizations and the like would be done on the desugared tree, and
keeping them separate allows us to add syntactic features without needing to
change the underlying implementation.

Of course, one potential pitfall of this is debugging: if we're using a
debugger and seeing different things than what we put in, that's not very
helpful. Similarly, exceptions and type error messages which reference some
desugared expression or whatnot might likely not be useful. However, we could
envision a "debugging mode" which would execute the original code directly;
assuming they're semantically equivalent, this would be fine.
