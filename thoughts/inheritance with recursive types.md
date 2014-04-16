Imagine we made a non-empty list:

```
object NEList a = Single a | Cons a (NEList a)

head : NEList a -> a
head = Single a -> a
     | Cons a _ -> a

tail : NEList a -> NEList a
tail = Single a -> Single a
     | Cons _ l -> l
```

Can we extend a recursive type with another recursive type?

```
object NEList a <: [! a] = 
  Single a <: [! a] 
| Cons a (next: NEList a) <: a :: next\super
```

Some things to note: we need access to the `super` attribute, which means that
we need to give the argument a name. Small price to pay, but it's not the greatest.

Of course, it's probably not actually that *useful* to do this anyway.
Since, of course, for most things with extension we just want to be able to 
use it "as the original one" when necessary...

A bit interesting though.

```
object Expr = Var Name | Lambda Name Expr | Apply Expr Expr

object ExprWithSrc <: Expr = 
    VarWithSrc <: Var
  | LambdaWithSrc <: Lambda
  | ApplyWithSrc <: Apply
  with source : (Int, Int)
```

The above seems nice and elegant, but only the top-level expression would actually carry any information:

```
expr_with_src = ApplyWithSrc (Var 'foo') (Var 'bar') with source=(1,1)
```

Because the inner expressions are just expressions, not containing any type data. What could we do?

```
object Expr = Num Int | Var Name | Lambda Name Expr | Apply Expr Expr

object ExprWithSrc <: Expr = 
    NumWithSrc <: Num
  | VarWithSrc <: Var
  | LambdaWithSrc n (e: ExprWithSrc) <: Lambda n e\super
  | ApplyWithSrc (e1: ExprWithSrc) (e2: ExprWithSrc) <: Apply e1\super e2\super
  with source : (Int, Int)
```

Now this is interesting, because `ExprWithSrc` is a recursive type on *itself*, not on an `Expr`. And every instance of an `ExprWithSrc` will contain inside it an `Expr` under the `super` attribute. So we have all the information we want, and we can use `ExprWithSrc` wherever we would use an Expr.

```
object Value = VNum Int | Closure Str Expr {d Str => Value}
eval env = Num n => VNum n
         | Var n => env n
         | Apply (Closure n e' env') e => 
            eval (env.union (env'.add n (eval env e))) e'
         | Lambda n e => Closure n e env
```
