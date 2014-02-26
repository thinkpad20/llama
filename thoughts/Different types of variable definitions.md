## Different types of variable definitions

Let's make `b = a`, so it inherits a's value and properties, but
only at that fixed point.

```
> a = mut 0
0
> b = a
0
> ++a
1
> b
0
> ++b
1
```

Now let's have `b` be a fixed copy of `a`:

```
> a = mut 0
0
> b = fix a
0
> ++a
1
> b
0
> ++b
Error: `b' is not mutable
```

Let's let `b` be a reference to `a`, that is, they both use the same
memory space. Mutating `b` will mutate `a`.

```
> a = mut 0
0
> b = ref a
0
> ++a
1
> b
1
> ++b
2
> a
2
```

Not let's have it be a `window`, aka a fixed reference. Its value changes,
but it cannot be modified directly. 

```
> a = mut 0
0
> b = window a
0
> ++a
1
> b
1
> ++b
Error: `b' is a fixed reference
```

We can only make `ref`s or `window`s onto variables or array references:

```
> a = window 0
Error
> a = ref Nothing
Error
> a = 0
0
> b = window a
Error? Or is it ok? Kinda pointless
> c = mut 0
0
> b = window c
Definitely makes sense
> foo = (n: window Int) => 
    prev = mut *n
    when n changes
      if n == 4 then break
      println "changed! Previous value was #{prev}, now value = #{n}"
    println "done watching"
(function: Int -> ())
> spawn foo 0
Error: can't window onto a constant
> spawn foo c
()
> c := 5
5
changed! Previous value was 0, now value = 5
> c := 3
changed! Previous value was 5, now value = 3
3
> c := 4
done watching
4
```
