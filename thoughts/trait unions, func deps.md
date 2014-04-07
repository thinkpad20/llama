## Trait unions

What if we broke the individual components of a trait down, so that we could be more granular? Ex:

```
trait Add a b -> c = 
  (+) : a -> b -> c

trait Subtr a b -> c = 
  (-) : a -> b -> c

trait Mult a b -> c = 
  (*) : a -> b -> c
```

So for example, with `Str`:

```
impl Add Str Str Str = s + s' = s.concat s'
impl Mult Str Int Str = s * n = result after
  result = mut ''
  for _ in n.times do result += s
```


So now `Str` satisfies `Add` and `Mult`, in its own way. But it doesn't satisfy `Subtr`, and it can't multiply with itself. Now we have a *separate* numeric trait `Num`, which is similar to the Haskell one. But for the `Num` trait, all of the types must be equal. We can simply specify this by a *union* of existing traits:
 
```
trait Num a = {Add a a a , Subtr a a a, Mult a a a}
```

Expanding out these definitions, we get

```
trait Num a = {
    (+) : a -> a -> a,
    (-) : a -> a -> a,
    (*) : a -> a -> a
}
```

Now we don't have to define new functions; we can simply say that a type which satisfies these three traits is also considered to satisfy `Num`. The two are equivalent.

We could also, for example, add in the `fromIntegral` method Haskell has for `Num`:

```
trait Num a = {Add a a a, Subtr a a a, Mult a a a, from_int : Int -> a}
```

How about, for example:

```
foo x y = x + y * (x - y)
```

What's the type of `foo`?

```
foo : (Add a b c, Subtr a b d, Mult c d e) -> a -> b -> e
```

Cray. But is it unsound? I don't think so, but IDK for sure.

### Example: Monoids.

```
trait Zero a =
  zero : a

trait Append a b -> c = 
  append : a -> b -> c

impl Zero Num = zero = 0
impl Append Num Num Num = append n m = n + m

trait Monoid a = {Zero a, Append a a a}
```

`Num` is already a member of `Monoid`, because of the above `impl`s.

```
blorp : Append a Num b => a -> b
blorp x = x.append 3

blorp : Num -> Num
```

### Example: Applicative/Monad.

```
trait Functor f = map : (a -> b) -> f a -> f b
trait Lift m = lift : a -> m a
trait Functor f => Applicative f = {
    Lift f, 
    apply : f (a -> b) -> f a -> f b
}
trait Bind m = bind: m a -> (a -> m b) -> m b
trait Monad m = {Applicative m, Bind m}
```

So it's appearing that specifying a trait is the same as specifying a set of functions which place restrictions on the type variables.
