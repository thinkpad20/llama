### Encoding natural numbers in Llama

```
type Nat = Z; S Nat

(+): Nat -> Nat -> Nat =
  Z   -> n -> n
  S n -> m -> S n+m

for (+): Nat -> Nat -> Nat
Z + n = n
S n + m = S n+m

(*): Nat -> Nat -> Nat = 
  Z   -> _ -> Z
  S n -> m -> m + n * m

(-): Nat -> Nat -> Nat =
  Z   -> 
    Z -> Z
    _ -> throw Exception('No negatives')
  S n -> S m -> n - m

convert: Int -> Nat =
  0 -> Z
  n -> S (convert n - 1)

convert: Nat -> Int =
  Z -> 0
  S n -> 1 + convert n

trait Convert a b = convert: a -> b

(==): Nat -> Nat -> Bool =
  Z == Z = True
  S n == S m = n == m

trait Eq a b = (==): a -> b -> Bool

trait Numeric t =
  Eq t t
  (*): t -> t -> t
  (+): t -> t -> t
  (-): t -> t -> t
  Convert Int t

```
