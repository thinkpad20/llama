```
a = mut 0
a := a + 1
a += 1
```

What is type of +=?

```
where exists (+: (a, b) -> a)
  (x: ref a) += (y: b) = x := x + y

(+=): (ref a, b) -> a where exists (+: (a, b) -> a)
```

Theoretically, this should hold up. However, this is weird because is it a restriction on `a`? `b`? `+`? Or on all three? It seems almost impossibru to type check this. On the other hand, we intuitively know immediately whether `+=` is a valid operation: `a += b` is valid if and only if there exists some function `+` which takes `a`'s type and `b`'s type, and returns `a`'s type. So we might be able to derive the type by the definition:

```
x += y = x := x + y
```

Now we're getting really into Hindley-Milner land, though. Well, we know that the type signature of `:=` is `(ref a, a) -> a` (with a side effect). So we unify `x` with `ref a` and `x + y` with `a`. Now we know that the return type is an `a`. Digging into `x + y: a`... more on this later.

Ugh, I really don't want to go full HM just yet. If it's this complicated to express the types, I'd rather just leave it as syntactic sugar. For now.

However, going back to that "intuitive" understanding that I expressed earlier: might we somehow be able to express that algorithmically?

```
is_valid(Apply (Var "+=") (Tuple [Var "a", Var "b"])) = 
  a_type = type_of (Var "a")
  b_type = type_of (Var "b")
  get_ret_type ("+", TTuple [a_type, b_type]) == Just a_type

get_ret_type(name, arg_type) = 
  func_type = lookup name
  case func_type of
    Function(from, to) => if from == arg_type then Just to else Nothing
    MultiFunction set => set.get arg_type
```

Meh. Let's leave it for now.
