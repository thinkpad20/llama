Some objects all share certain things in common; we can keep these things
but not let them be used in patterns (except as boolean qualifiers)

```
object Expr =
  Number Double
  String String
  Var Name
  Call (Expr, [Expr])
  Lambda Name Expr

  sharing
    lineNumber = 0
    columnNumber = 0
    fileName: String


> e1 = Number (2, lineNumber=1)
> e2 = Number (5.5, lineNumber=2)
> e3 = Call (Var "+", [e1, e2], lineNumber=3)
> e2.lineNumber
2
> e1.columnNumber
0
> case e3 of Call(_, _) => print e3.fileName
None
> case e1 of Number n if e1.lineNumber == 1 => print "hello"
hello
```

Multiple dispatch smooths out issues of floating-points vs
integers

```
(n: Int) + (m: Int) = [BUILTIN/+(Int, Int)]
(n: Float) + (m: Float) = [BUILTIN/+(Float, Float)]
toFloat (n: Int) = [BUILTIN/toInt(Float)]
floor (f: Float) = [BUILTIN/floor(Float)]
(i: Int) + (f: Float) = toFloat i + f
(f: Float) + (i: Int) = toFloat i + f
```

Short-circuit evaluation

When calling a function that takes a "lazy" argument, the argument will be
wrapped in a `Thunk` wrapper and not be evaluated (unless already evaluated)
unless/until needed

```
(a: Bool) || (b: lazy Bool) = if a then True else b
(a: Bool) && (b: lazy Bool) = if !a then False else b
if_ (c: Bool) (t: lazy a) (f: lazy a) = if c then t else f

> print if_ False (1/0) (3)
3
```