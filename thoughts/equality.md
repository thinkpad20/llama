```
id1: a -> a = 
  x -> println 'id1!'; x

fact: Int -> Int = 
  0 | 1 -> 1
  n -> n * fact n-1
```

A few ways to declare a list equality function:

```
(==): List a -> List a -> Bool with {Eq a a} = 
  [] -> [] -> True
        _ -> False
  Cons a l1 -> Cons b l2 -> a == b && l1 == l2
                       _ -> False

(==): List a -> List a -> Bool with {Eq a a} = 
  l1 -> l2 -> case l1, l2 of
    [], [] -> True
    Cons a l1, Cons b l2 -> a == b && l1 == l2
    _, _ -> False

with Eq a a
l1:List(a) == l2:List(a) : Bool = case l1, l2 of
  [], [] -> True
  Cons a l1, Cons b l2 -> a == b && l1 == l2
  _, _ -> False
```

```
(r: Ref a)! : a = <BUILTIN>
(m: Maybe a)! : a = case m of
  Just a -> a
  Nothing -> throw Error('Cannot unbox `Nothing`')

with Default a
(m: Maybe a)!: a = case m of
  Nothing -> default
  Just a -> a

trait Unbox c a = (_!): c a -> a

with Unbox x a, Num a, Functor f
addall: f (x a) -> a = map (_!) ~> sum
```
