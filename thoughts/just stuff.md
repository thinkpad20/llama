type Expr 
  Var Str
  Num Float
  Let Str Expr Expr
  Lambda Str Expr
  Apply Expr Expr

type ParsedExpr <: Expr
  with line: Int; column: Int

===

type ParsedExpr <: Expr
  with line: Int, column: Int

type ParsedExpr <: Expr =
  PVar <: Var
  PNum <: Num
  PLet <: Let
  PLambda <: Lambda
  PApply <: Apply
  with line: Int = 1, column: Int = 1

e1 = PVar 'foo'
e2 = e1 with line=1, column=2

type DesugaredExpr = 
  DVar Str
  DNum Float
  DLambda Str Expr
  DApply Expr Expr
  with original: Maybe ParsedExpr = Nothing

desugar : ParsedExpr -> DesugaredExpr
desugar p = wrap (res p) after 
  wrap d = d with original = Just p
  res =
    PVar n -> DVar n
    PNum n -> DNum n
    PLambda s e -> DLambda s e
    PApply e1 e2 -> DApply e1 e2
    PLet n a b -> DApply (DLambda n b) a


> type Foo with a: Int, b: Str
> type Bar with a: Str, b: Int
> a (foo: Foo) = foo\a
> b (foo: Foo) = foo\b
> a (bar: Foo) = bar\b
> b (bar: Foo) = bar\b
> :t a
a : Foo -> Int 
a : Bar -> Str
> :t (Foo with a=10, b='hey').a
(Foo with a=10, b='hey').a : Int
> :t (Bar with a='hey', b=10).a
(Foo with a=10, b='hey').a : Str


```
type Foo Int Str with a: [Int], b: Float
foo = Foo 0 'hey' with a=[1,2,3], b=10.01
```

this is analogous to

```
typedef Foo = (Int, Str; a: [Int], b: Float)
foo = (0, 'hey'; a=[1,2,3], b=[10.01])
```

Both tuples and single-constructor ADTs have the same information, except that ADTs also carry a constructor name. We can write a function which "tuple-izes" any single-constructor ADT, although it looks like this would be a dynamically typed function and/or at the template level.







