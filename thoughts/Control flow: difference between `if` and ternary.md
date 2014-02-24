Control flow: difference between `if` and ternary in C is that ternary can't change the control flow. `if` can.

```c

int fact1(int n) { 
  return n < 2 ? n : n * fact(n - 1); 
}
int fact2(int n) { 
  if (n > 10) {
    puts("wow, that's a big number");
    return -1;
  }
  if (n < 2) {
    puts("done!");
    return n;
  } else {
    puts("recursing...");
    return n * fact(n - 1); 
  }
}

```

```
fact1(n: Int) = if n < 2 then n else n * fact(n-1)
fact2(n: Int) = 
  if n > 10
    print "wow, that's a big number"
    return -1
  if n < 2
    print "done!"
    n
  else
    print "recursing..."
    n * fact(n - 1)

```

Key thing is that the `return` statement kinda fucks things up. Not to mention `break` and `continue`. But it's very useful to have when desired.

That's the idea behind statements, though... they don't always have return values. For example, what's the return value of `break`? `continue`?

What if we have it so that the `for` and `while`s always are of type (), no matter what, useful only for their side-effects? That's the normal approach...

```
foo(v: [Int], predicate: Int -> Bool, @ignore={}) = 
  bad_is = mut []
  result = mut 0
  for i in vec
    if i in ignore then continue
    if i < 0 then throw Exception()
    if i == 10 then return -10
    if predicate i
      print "#{i} satisfies the predicate!"
      result := i
      break
    else
      print "#{i} doesn't satisfy the predicate"
      bad_is.append! i
      result -= i
  print "Bad i's: #{bad_is}"
  result
```