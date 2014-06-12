type Nat = Z; S Nat

(+): Nat -> Nat -> Nat =
  Z -> n -> n
  S n -> m -> S (n + m)

# Captures x. x will ALWAYS be 1 when f is called, and the `+` is fixed as well.
x: Nat = 1
f (y:Nat) Nat = x + y

# Doesn't capture x; can be imported into a module with a different x.
with x:Nat 
g (y:Nat) : Nat = x + y

# Doesn't capture x or +.
with x:Nat, (+): Nat -> Nat -> Nat
h (y:Nat): Nat = x + y

# Polymorphic version of above.
with x:a, (+): a -> b -> c
h (y:b): c = x + y
