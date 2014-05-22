```
my_func a = case a\type of
  Str, [_], [!_] -> a.reverse
  Num -> a + 10
```

type of my_func is `? -> ?`
but really we could tighten it up, and say

```
> my_func : a in {Str, [_], [!_], Num} -> a
> my_stuff = [1, ['hello'], 'goodbye', [!1, 2, 3]]
> my_stuff.each x => println my_func x
11
['hello']
'eybdoog'
[!3, 2, 1]
```

We should be able to modify the gradual typing rules to allow sets of types. I hope. The basic idea would be that the *?* type is consistent with *any* type, while a type set *S* is consistent with any type *in that set*. It would of course require more thought. We'd want to get the benefits of type propagation. For example, even regardless of type sets, we'd like to be able to do this:

```
id : a of ? -> a
id x = x
```

If we could do this, it would be the same as a trait constraint. For example, let's say that only two types, `Num` and `Str`, implement the `Show` trait. Let's look at some functions:
```
f1 x = x.show <> '!!'
f2 x = (x.show, x)
f3 x = case x\type of
  Num => (x.show, x + 1)
  Str => (f1 x, x.length)
f4 x = case x\type of
  Num => (x.show, x + 1)
  Str => (f1 x, x.length)
  _ => x
```

We can type the first function as `f1 : _ of Show -> Str`. We don't need to give the initial type a name, because it will get discarded immediately. However, in the second function: `f2 : a of Show -> (Str, a)`. In this case, we want a name, because we want to record that it's *that type*, not just any type, which will be the type of the second element of that tuple. How about the third function? It's type is dynamic: `f3 : ? -> (Str, Num)`. But we could narrow it down: `f3 : {Num, Str} -> (Str, Num)`. But it's not clear that we can do this by simply exposing the type as an attribute. But perhaps. Now how about the third type? It seems completely dynamic. We can't be sure that it will return a `(Str, Num)`. Technically we could give it this type: `f4 : {{Num, Str} -> (Str, Num), a -> a}`. But trying to sanely manage all of this would be horrible. It's probably better to simply not support this, and have something simpler.

Having different syntaxes for arrays, vectors and lists is annoying. How about:

```
trait Listable t with
  from_list : List a -> t a
  to_list : t a -> List a

implement Listable for List with from_list = id

implement Listable for Array with
  from_list list = result after
    result = ref Array\with_length list.length
    for i, x in list.enumerate do result[:i] := x

implement Listable for Vector with
  from_list list = result after
    result = ref Vector\Empty
    for i, x in list.enumerate do result .= append x

trait Reverse x with 
  Listable x
  reverse : x -> x = from_list ~> reverse_list ~> to_list
```

One issue with the above is that it calls for multi-parameter traits. Really we should have `trait Listable a t with ...`. Then a literal of the form `[1, 2, 3]` would be of type `_ of Listable Num`. Implementing multiparam type classes is hard though, which means we should probably either wait, or simply leave it as dynamic. We could imagine the traits being parameterized; e.g. `[1, 2, 3] : _ of Listable<Num>`. Starts to get wack. Would probably be simplest in the beginning at least to restrict traits to only looking at the outer-most type.

An even cooler dynamic thingie we could try is:

```
trait Reverse a = 
  reverse (x: a): a = case x\type of
    List _ => reverse_list x
    _ of Listable => x.to_list.reverse.from_list

# Vector takes the default reverse trait
implement Reverse for Vector _

# This will type check at first but throw a runtime error, 
# because Num is not an instance of listable
implement Reverse for Num

# This will succeed, even though it doesn't make much sense
implement Reverse for Num with reverse = negate

# This will succeed, and it makes sense, even though it's a different kind.
implement Reverse for Str with reverse = reverse_str

foo = [1, 2, 3] # foo is of type _ of Listable Num, if we can support that
println reverse foo
println reverse $ 1 + 2 * 3
```

