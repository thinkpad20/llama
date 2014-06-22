Some useful properties:

```
# E.g. lists, maybes, anything which could be empty
trait Possible t = isempty: t _ -> Bool

# Objects which contain singleton values.
trait Get t = get: t a -> a

# Objects which can access the next element.
trait Next t = next: t a -> t a
```

Seems nice! What if we combine some of them?

```
# Things that act like iterators
trait Iterator t = Possible t, Get t, Next t

# Things which can be iterated over
trait Iterable t = iter: t a -> Iterator a

# Containers
trait Container t = Lift t, Iterable t
```

Pretty cool. For an example, there's `List`:

```
isempty: List a -> Bool =
  End -> False
  _ ~ _ -> True

get: List a -> a =
  End -> throw "Empty list"
  x ~ _ -> x

next: List a -> List a =
  End -> throw "Empty list"
  _ ~ rest -> rest

iter: List a -> List a = id
```

In particular, the `Iterable` trait is used for loops:

```
print_all: a -> () with {Iterable a} = x ->
  my_iter = ref x.iter
  while my_iter.valid
    println my_iter.get
    my_iter .= next

print_all [1..1000]
print_all {Random.randstr 10 for _ in 100.range}
```
