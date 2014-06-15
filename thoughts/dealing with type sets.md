Here's a formulation of a type set of Llama types (in putative Llama code):

```
type TypeSet (predicates: {(Name, Type)}) (types: {Type})
```

Here are some examples of type sets:

```
x: Int = 1
x: Str = 'hello'
x: Str -> Int = length
# typeset(x) = TypeSet {} {Int, Str, Str -> Int}

trait Lift f = lift: a -> f a
# typeset(lift) = TypeSet {(lift, a -> f a)} {a -> f a}
```

We can apply types to each other (e.g. `Maybe Int`, `List Str`), so we should be able to do the same for type sets:

```
apply: (TypeSet p1 ts1 : TypeSet) (TypeSet p2 ts2 : TypeSet): TypeSet =
  TypeSet p1+p2 {t1 t2 for t1 in ts1, t2 in ts2}
```

If there are no predicates, it's simply pairwise combinations of the two sets.

```
assert (
  TypeSet {} {Maybe, List} .apply TypeSet {} {Int, Str, Int->Str}
    == TypeSet {} {Maybe Int, Maybe Str, Maybe (Int -> Str),
                   List Int, List Str, List (Int -> Str)}
  )
```

If there are predicates, we retain those:

```
assert (
  TypeSet {(lift, a -> f a)} {f} .apply TypeSet {} {Int, Str}
    == TypeSet {(lift, a -.> f a)} {f Int, f Str}
)
```

As type sets are analogous to types, they can contain quantified variables (polytypesets? schemesets?). It's an open question, though, as to whether the quantification should be *outside* the set or inside. For example:

```
id: Str = 'some_unique_id'
id (x:a): a = x
# => id : TypeSet {} {Str, ∀ a. a -> a}
```

But this introduces an interesting question:

```
foo: Int = 1
foo {AddSelf a} => (x:a): a = x + x
foo (x:a) : a = x
#=> foo : TypeSet {Int, ∀a. a -> a, ∀a. {(+): a -> a -> a} => a -> a}
```

Ugh...

Let's look at an example

```
foo: Vector t -> t = head
foo: List t -> t = head
foo: String -> Char = s -> s 0
foo: String -> String = s -> s 0
foo: {Show a} => a -> Int = .show.(+ '!').length
```

```
foo: {∀t. Vector t -> t,
      ∀t. List t -> t,
      String -> Char,
      String -> String,
      ∀a. {exists {show: a -> String}} => a -> Int}
```

But this even suggests that type sets should contain other type sets! Blaaaaargh
