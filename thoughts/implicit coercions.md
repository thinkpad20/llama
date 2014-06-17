We can think of a fairly good hierarchy from booleans, to natural numbers, to rationals, to floating-point numbers. At each step, we have strictly more information than we did before, so we can always map from the previous to the next (though not necessarily vice-versa). A way to think of this is that if `child <: parent` for types `child` and `parent`, then there exists a function `coerce: parent -> child` which cannot error. For example:

```
trait Coerce a b = coerce: a -> b
coerce: Bool -> Nat =
  False -> Z
  True -> S Z
coerce: Nat -> Int =
  Z -> 0
  S n -> 1 + coerce n
coerce: Int -> Rational = (// 1)
coerce: Fractional -> Float = <BUILTIN>
```

Where we have assumed that `type Rational = Int // Int`.

This is the reverse of the parent-child relationship in OO-style hierarchies. In that system, we can always convert a child into its parent, not the other way around (because the child contains "more information" that the parent might be missing). For example, viewing `Either b a` as an extension of `Maybe a`, we can always convert an `Either` into a `Maybe`, but we can only do the reverse if the `Maybe` value is a `Just`.

In particular, a primary reason for subtypes in OO is to let us reuse code: if a function operates on an `A`, and `B` extends `A`, then that function will also work on a `B` (similarly for properties of `A`).

It seems that we should be able to generalize type hierarchies. In particular, it doesn't really *matter* what is a parent or what is a child. What matters is, does there exist a function which can convert from one type to another? If we can guarantee a conversion from type `A` to type `B`, and `f: B -> t`, then we can use `f` on an `A`: we simply have to convert from `B` first. A good question then, is if `f` *returns* a `B`, can we treat it as an `A`? These are two different questions, one for each direction of conversion.

For example, consider the function

```
_if: Bool -> a -> a -> a
_if c t f = if c then t else f
```

(Ignoring laziness for the time being). If a type can be *viewed* as a boolean, then we can use it as a condition for `_if`. For example, `""`, `0`, `Nothing`, etc might be viewed as `False`, while a non-empty string, a non-zero number, or a `Just` value are true. What if we wanted to generalize `_if`?

```
trait Boolable t = tobool: t -> Bool
_if : Boolable a => a -> b -> b -> b
_if c t f = if tobool c then t else f
```

So this is cool! `List`, `String`, `Maybe`, `Int`, etc can all be viewed as booleans. But now we have to write a new trait every time we want to do something like this. If we wanted to generalize `Maybe`s ("things which can either have a value or be empty"), we'd have to write a `Maybeable` trait. What if we could generalize it?

```
trait Coerce a b = coerce: a -> b
```

First now note that `Boolable` and `Maybeable` are just special cases of `Coerce`:

```
trait Condition t = Coerce t Bool
trait Possible t = Coerce (t a) (Maybe a)
```

So this is quite useful:

```
_?: Condition t => (x:t) : Bool = coerce x

loop: Condition t => (vec: [t]): [t] =
  result = ref []
  for x in vec do unless x? then break else result += x
```

But perhaps we could get even smarter: we could this automatically. This would let us, for example, pass coercible types freely around.

```
print_maybe: Show a => Maybe a -> () =
  Nothing -> println "Nothing there."
  Just x -> println "It's a #{x}!"

print_maybe Nothing
print_maybe (Just 4)
print_maybe (Left "this will be ignored")
print_maybe (Right "this will be printed")
```

Perhaps the compiler could be smart enough to do this automatically? Then again, it might be better to be explicit.

A particular case where this is nice is wrapper types:

```
type StrList (List Char)
my_str_list = StrList 'hello'.tolist
```

First of all, we could generalize this up by writing `my_str_list = StrList 'hello'.coerce`. But we're seeing the issue, which is that a `StrList` is structurally exactly the same as a `List Char` (in Haskell it would be a `newtype`), and yet we can't use any of the functions that would work on `List Char`:

```
reverse: List a -> List a = rev [] after
  rev acc = [] -> acc | x ~ rest -> rev x~acc rest

reverse: StrList -> StrList = StrList cs -> StrList cs.reverse
```

So we have to rewrite `reverse` even though it's exactly the same, just with the wrapper removed and added. It would be nice to be able to skip that, and have the compiler do just that: unwrap the constructor and rewrap it.

How about if we had

```
coerce: StrList -> List Char = StrList cs -> cs
coerce: List Char -> StrList = StrList
```

Now we know how to get from a `StrList` to `List Char` and back. Now we could either do this entirely automatically, which might not be a great idea in all cases (but might), or we could have some kind of syntactic sugar:

```
my_str_list = 'hello'.coerce
assert (reverse |my_str_list| == StrList 'olleh')
```

Where the syntax `f |x|` would sugar `f(x.coerce).coerce`

Alternately, we could do it all with functions; e.g.

```
~_ = coerce
assert (~reverse(~my_str_list) == StrList 'olleh')
```

This might be the best for now, because it uses functions instead of syntactic sugar. All interesting things to think about.
