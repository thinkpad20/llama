trait Add a b c = _+_: a -> b -> c
_+_: Nat -> Nat -> Nat = 
  Z -> n -> n
  S n -> m -> S (n + m)

_+_: Int -> Int -> Int = <BUILTIN>

type Point a (x: a) (y: a)
_+_: {Add a a a}. Point a -> Point a =
  Point x1 y1 -> Point x2 y2 -> Point x1+x2 y1+y2

∴ instances of Add are:
  {}. Nat Nat Nat
  {}. Int Int Int
  {Add a a a}. (Point a) (Point a) (Point a)

so if we write a function:

foo x = x + x

then `foo: {Add a a b}. a -> b`

So what if we call `foo Z.S`? Well:

```
typeof (foo (S Z))
-> typeof foo
  -> {Add t0 t0 t1}. t0 -> t1
-> typeof (S Z)
  -> typeof S
    -> Nat -> Nat
  -> typeof Z
    -> Nat
  -> res = t2
  -> subs = unify (Nat -> Nat) (Nat -> t2) = {t2 => Nat}
  -> ({t2 => Nat}, Nat)
-> res = t3
  -> subs = unify (t0 -> t1) (Nat -> t3) = {t0 => Nat, t1 => t3}
-> ({t2 => Nat, t0 => Nat, t1 => t3}, {Add Nat Nat t3}. t3)
```

It *seems* like we should be able to take `{Add Nat Nat t3}`, and the facts that (1) we've run out of further things to type, and (2) that the only matching type tuple is `Nat Nat Nat` to infer that t3 is a `Nat`.
