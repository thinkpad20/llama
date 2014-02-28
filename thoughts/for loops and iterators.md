Let's say that the following code:

```
for x in y do z x
```

Is equivalent to the following code:

```
iter = y.iter
while iter.valid
  x = iter.get
  z x
  iter.forward!
```

This is similar to Java's `for (_ : _)` pattern. Similarly, it suggests an iterator trait:

```
trait Iterable a =
  iter : Self -> iterator of Iterator Self a

```

The thing is, just what makes an iterator for any given data type changes, which suggests that the iterator *itself* is simply just some object which satisfies a trait:

```
trait Iterator container item = 
  valid : Self -> Bool
  get : Self -> item
  forward : Self -> Self
  forward! (iter: ref Self) = iter := iter.forward
```

So then the kinda tough thing is, when we call `iter` on an object, and we know that that object satisfies the `Iterable` trait, then we know that we're getting back *some* `i` which satisfies the trait `Iterator`. Further, the `Iterator` trait itself is paramaterized by two things: what the iterator is iterating over, and what the iterator will return when it calls `get`.

I'm not *entirely* sure that it's possible to keep this straight in a Hindley-Milner world, that is without crazy type/kind/forall gynmastics. But let's see if it might be conceivable:

```
[l a] as Iterable =
  iter list = list
[a] as Iterable = 
  iter vec = VectorIterator vec

object VectorIterator [a] = 
  property index: Num
  pattern VectorIterator (vec: [a])
    index = 0

[l a] as Iterator [l a] = 
  list.valid = case list of [l] => False | _ => True
  list.get = case list of x~_ => x | _ => throw Error "empty list"
  list.forward = case list of _~rest => rest | _ => throw Error "empty list"

VectorIterator [a] as Iterator [a] =
  vi.valid = vi.index < vi.vec.length
  vi.get = vi.vec vi.index
  vi.forward = VectorIterator (vi.vec, index = vi.index + 1)
```

Hmm, this might be doable, possibly with great effort. It's quite possible, of course, that we'll be able to handle all of this if we insist on enough type annotations (and we can always do this to start with, and beef it up as it goes forward). Regardless, though, this makes it clear that we need higher-order types.

One thing that gets a little fuzzy though, is that our definition of the Iterable trait suggests a type with kind `* -> *`, but the `Iterator` trait suggests a type with kind `* -> * -> *`. And yet, we were "able" (it appears) to satisfy both traits with the same type, namely `[l a]`. This is important to lock down.
