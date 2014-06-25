Regular memoizing function with mutable hash:

```
memoize : Ord a. (a -> b) -> a -> b
memoize f =
  memo = {=>}
  arg ->
    if memo.has arg then memo[arg]
    else memo[arg] := f arg

fib = memoize
  0 -> 0
  1 -> 1
  n -> fib n-1 + fib n-2
```

Initializing the memo with some values:

```
memoize_with : Ord a. HashMap a b -> (a -> b) -> a -> b
memoize_with memo f = arg ->
  if memo.has arg then memo[arg]
  else memo[arg] := f arg

fib = memoize_with {0=>0, 1=>1} (n -> fib n-1 + fib n-2)
```

Using a kwarg:

```
memoize : Ord a. (a->b; memo: HashMap a b) -> a -> b =
  (f; memo={=>}) -> arg -> 
    if memo.has arg then memo[arg]
    else memo[arg] := f arg

fib = memoize(f; {0 => 0, 1 => 1}) after f n = fib n-1 + fib n-2
```

An object-oriented approach:

```
type Memoizer a b = Memoizer (f: a -> b) with
  memo: HashMap a b

impl @Call Ord a. (Memoizer a b) a b with
  @call m arg = 
    if m\memo.has arg then m\memo[arg]
    else m\memo[arg] := m\f arg
```
