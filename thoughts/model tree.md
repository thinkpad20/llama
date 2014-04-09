
```
object Model = Root | Sub String Object

sort_models models = result.to_vector after
  result = ref UniqueVector []
  loop m = case m of 
      Model =>
        result.append! m
    | SubClass _ parent =>
        loop parent
        result.append! m
  map loop models
```

By the way, what does the `UniqueVector` look like?

```
object UniqueVector t = UniqueVector (v: [t])
  with elems : {t}

# Using only attributes, no pattern matching
append! uv x = uv after
  if not uv\elems.contains x then uv\v.append! x

# Using `aka` and pattern matching
append! (uv aka ref UniqueVector v) x = uv after
  if not uv\elems.contains x then v.append! x

# Using pattern matching on attrib
append! (ref UniqueVector v with elems=elems) x = uv after
  if not elems.contains x then v.append! x
```

The first example brings up an important point, the age-old question of values vs references: we should 'know' that `uv` is a reference. How do we know that? well, we call `uv\v.append!`, which means that `uv\v : ref [a]`. But couldn't we have simply written it as

```
object UniqueVector t = UniqueVector (v: ref [t])
  with elems : {t}
```

?

We need to suss this out. Err on the side of python. For example, we could simply make all variables references, as they do in Scala (and nearly so in Java).

```
> foo1 = 1
> foo2 = mut 1
> foo3 = ref 1
> bar (n: mut Int) = n += 1 # pure function
> print "#{bar foo1}, #{foo1}"
2, 1
> print "#{bar foo2}, #{foo2}"
2, 1
> print "#{bar foo3}, #{foo3}"
2, 1
> baz (n: ref Int) = n += 1 # local function
> print "#{baz foo1}, #{foo1}"
Error: foo1 is not a reference
> print "#{baz foo2}, #{foo2}"
2, 2
> print "#{baz foo3}, #{foo3}"
2, 2
> qux n = n += 1
Error: Definition is ambiguous: n could be mut Int or ref Int
```

One thing that we should be able to do is simply map all variables to IORefs, so that they're inherently mutable... but this doesn't solve the typing woes. The original idea that we had, which is that `mut`s can be transformed into references, but aren't the same as references, seems promising. But yeah, more thought on this is required. Keep in mind the C++ system which separates, variables, pointers and references. Variables can be coerced into references in that system (which is compatible with our view because C++ variables are mutable).

```c++
#include <iostream>
using namespace std;

int bar(int i) {
  i += 1;
  return i;
}

int baz(int &i) {
  i += 1;
  return i;
}

int main(int argc, char const *argv[]) {
  int _one = 1;
  cout << bar(_one) << "  " << _one << endl; // 2  1
  cout << baz(_one) << "  " << _one << endl; // 2  2
  int &a_ref = _one;
  cout << bar(a_ref) << "  " << a_ref << endl; // 3  2
  return 0;
}
```

Importantly, under the hood, C++ is converting `baz` to the function

```c++
int baz(int *i) {
  *i += 1;
  return *i;
}
```

And converting the calls to it:

```c++
  cout << baz(&_one) << "  " << _one << endl;
```

The underlying point is that we want the type checking and reference system to make our lives easier, not more difficult. If it doesn't make the language easier to use, more fun, more readable, etc, then strongly consider dropping it.
