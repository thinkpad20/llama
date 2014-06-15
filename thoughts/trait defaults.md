
```
trait Add a b c = (+): a -> b -> c
```

Declaring this means that `(+): forall a, b, c. Add a b c => a -> b -> c`.

Then if we were typing the expression `1 + 2`:

```
typeof (1 + 2)
  first typeof (+ 1):
    typeof + -> inst(forall a, b, c. {Add a b c} => a -> b -> c) 
                     = {Add t0 t1 t2} => t0 -> t1 -> t2
    typeof 1 -> inst(forall a. {IntLiteral a} => a)
                     = {IntLiteral t3} => t3
    result = newvar = ({} => t4)
    subs = unify ({Add t0 t1 t2} => t0 -> (t1 -> t2), 
                  {IntLiteral t3} => t3 -> t4)
         = {t0 => t3, t4 => (t1 -> t2)}
    apply subs result = {Add t3 t1 t2, IntLiteral t3} => t1 -> t2
    = ({Add t3 t1 t2, IntLiteral t3} => t1 -> t2, {t0 => t3, t4 => (t1 -> t2)})
  typeof 2 -> inst(forall a. {IntLiteral a} => a)
                     = {IntLiteral t5} => t5
  result = newvar = ({} => t6)
  subs = unify ({Add t3 t1 t2, IntLiteral t3} => t1 -> t2, 
                {IntLiteral t5} => t5 -> t6)
       = {t1 => t5, t2 => t6}
  apply subs result = {Add t3 t5 t6, IntLiteral t3, IntLiteral t5} => t6

```

So it looks like we have intermediate types which are ambiguous. This is the reason for functional dependencies in Haskell. If we had instead

```
typeof (1:Int + 2:Int)
  first typeof (+ 1:Int):
    typeof + -> inst(forall a, b, c. {Add a b c} => a -> b -> c) 
                     = {Add t0 t1 t2} => t0 -> t1 -> t2
    typeof 1:Int -> 
      typeof 1 -> inst(forall a. {IntLiteral a} => a)
                       = {IntLiteral t3} => t3
      unify ({IntLiteral t3} => t3, Int)
      # Seeing if `t of IntLiteral` unifies with `Int` means checking if `Int`
      # implements `IntLiteral`. Of course, it does. So we can say:
      = (Int, {})
    result = newvar = ({} => t4)
    subs = unify ({Add t0 t1 t2} => t0 -> (t1 -> t2), Int -> t4)
         = {t0 => Int, t4 => (t1 -> t2)}
    apply subs result = {Add Int t1 t2} => t1 -> t2
    = ({Add Int t1 t2} => t1 -> t2, {t0 => Int, t4 => (t1 -> t2)})
  typeof 2:Int -> 
    typeof 2 -> inst(forall a. {IntLiteral a} => a)
                     = {IntLiteral t5} => t5
    unify ({IntLiteral t5} => t5, Int)
    = (Int, {})
  result = newvar = ({} => t6)
  subs = unify ({Add Int t1 t2} => t1 -> t2, 
                Int -> t6)
       = {t1 => Int, t2 => t6}
  apply subs result = {Add Int Int t6} => t6
```

One thing that might make this nicer is to specify "default types" for a trait. As in, "if this would result in ambiguity, use this type". That way the expression `1` would default to `Int`, `1.0` to `Float`, etc. Then the expression `1 + 2`, absent of other information, would go to `1:Int + 2:Int`, and if `+` defaulted to `Int -> Int -> Int`, then we would get `1 + 2 : Int`.

The idea might be one of "functional suggestions", a weaker version of functional dependencies. So you could have an instances of `Add Int Int Float` and `Add Int Int Int`, and but the default would be `Int -> Int -> Int`, clearing up most ambiguity.

```
default implement Add for Int Int Int = (+) => add_int

trait Add' a = Add a a a
```



foo: Nat -> Nat = (+ 1)
