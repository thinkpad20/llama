```
(+): Nat -> Nat -> Nat =
  Z -> n -> n
  S n -> m -> S (n + m)

x: Nat = 1
x: Str = 'hello'
y -> x + y
```

When we type `y -> x + y`:

```
# env = {x: {Nat, Str}, (+): {Nat -> Nat -> Nat}}
typeof env (y -> x + y)
   # env1 = {x:{N, S}, (+):{N->N->N}, y:{a}}
-> typeof env1 (x + y)
    -> typeof env1 (+ x)
      -> typeof +
        -> {N->N->N}
      -> typeof x
        -> {N, S}
      -> b = newvar
      -> find a type t in types(@call) where cbu(t, {N->N->N}->{N,S}->b)
      ->
         (Callable {N->N->N} {N,S} b)
-> v <- newvar
-> ts, subs <- typeof (env + {y=>{v}}) (Apply ("+"))
->
```




So clearly we need a way to answer some questions:

given type sets TS1 and TS2, create a new typeset TS* which represents all possible outcomes of TS1 TS2. For example,

TS1 = {Maybe, List}
TS2 = {Int, Num, Str}
TS* = {Maybe Int, Maybe Num, Maybe Str, List Int, List Num, List Str}

(If the type sets are inexplicit, this question might be murkier)

Yikes!

Given two type sets TS1 and TS2, does there exist t1 in TS1 and t2 in TS2 with cbu(t1, t2)?
