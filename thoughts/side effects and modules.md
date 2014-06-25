Detecting side-effects: we'd ideally like to be able to support the restriction of side-effects when desired. A side effect is either performing IO, or mutating a reference.

A function which does either of these things is impure. However, the effect only happens if the function is run; this means that assigning that function into a variable is a pure action.

```
pure_thing = 
  foo = -> println 'hey'
  bar = foo
  1
```

In the above code `pure_thing` is pure, even though it uses `println` internally; this is because the code isn't called, so no side-effects are performed.

Sorting out when this does and doesn't occur -- is it possible?

## Modules vs functions

A different but perhaps related question is exporting things. Modules and functions are really quite similar things: they can both be parameterized by inputs; they both have the ability to define things; they both implicitly define a new lexical scope; they both capture some sort of environment; they both can be seen as pure or impure. Similarly, neither one performs any actions gets used if it doesn't get used: for modules, "using" means importing, and for functions, "using" means calling.

One could say the difference with modules is that they don't return anything, but really a module returns a set of values: the values it exports.

Look at these "modules":

```
module a
  x = 1
  module b
    x = 2
    println x # prints 2
  println x # prints 1
```

Compare to:

```
a = 
  x = 1
  b = 
    x = 2
    println x # prints 2
  println x # prints 1
```

It looks essentially the same to me. In fact, I really think they might be the same. Now in ML, modules are special because they are parameterized by types. But I'm not sure we need this in Llama.

We could still use modules, but more as syntactic sugar for functions.
