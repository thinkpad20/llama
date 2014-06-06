We can put some crazy trait together on the fly:

```
with convert: b -> a, Eq a b
foo: (list: List a) (x: b): List a = reverse result! after
  result = ref End
  for elem in list
    if convert x == elem then throw Exception('Ooga booga!')
    result .= Cons(elem)
```

What we're essentially seeing is that a trait defines a relationship between types. It's saying "`a` and `b` are types such that there exists a function `convert` from `b` to `a`, and a function `==` from `a` to `b` to `Bool`." These relationships can be seen as logical assertions, so the function signature of `foo` can be read as:

> Given that `a` and `b` are types such that there exists a function `convert` from `b` to `a`, and a function `==` from `a` to `b` to `Bool`, and given that `list` is of type `List a`, and `x` is of type `b`, then `foo list x` is of type `List a`.

This is a precise logical statement, and can be evaluated for truth or contradiction by looking at the subsequent expression.
