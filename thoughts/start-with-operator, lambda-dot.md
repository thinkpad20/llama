### Lambda-dot and Start-With-Operator Syntactic Sugars

Let's say we have this code:

```
trait Monad (m: t -> t) = 
  bind : m a -> (a -> m b) -> m b;
  lift : a -> m a;

impl Monad for Maybe = { 
  m.bind f = case m of
      Just x => f x
    | Nothing => Nothing;
  lift x = Just x;
}


pipe m = ((m.bind $ n => lift (n + 1))).bind $ n => lift (n * 4)
assert pipe1 (Just 1) == Just 8
```

This is ugly! Too many parens, not readable. What can we do?

### Lambda-Dot

The first idea is lambda-dot: sugaring `x => foo x bar` to `.foo bar`.

```
sub (n: Num) (m: Num) = n - m
subFrom5 = sub 5
sub4From n = n.sub 4
sub4From' = .sub 4

assert 5 - 4 == sub 5 4 == subFrom5 4 = sub4From 5 == sub4From' 5 == 1
```


### Start-With-Operator

The second idea is that lines starting with a *binary operator* desugar into `(result of previous line (op) rest of line)`:

```
foo = {
  1;
  + 2 + 3;
  * 9;
  + 17;  
}

bar = (((1) + 2 + 3) * 9) + 17

assert foo == bar == 71
```

Then we could just use the forward application operator `|>` to simulate a Haskell `do`-block:

```
pipe m = {
  m;
  |> .bind (n => lift (n + 1));
  |> .bind (n => lift (n * 4));
}
```

Using `rassoc` we can make it even nicer:

```
rassoc .bind, lift
pipe3 = {
  m;
  |> .bind n => lift n + 1;
  |> .bind n => lift n * 4;
}
```

This becomes

```
pipe m = {
  m;
  |> (x => x.bind n => lift n + 1);
  |> (x => x.bind n => lift n * 4);
}
```

Which equals:

```
pipe m = (m |> x => x.bind n => lift n + 1) |> (x => x.bind n => lift n * 4)
```

Which equals:

```
pipe m = (m.bind (n => lift n + 1)).bind (n => lift n * 4)
```

Which equals:

```
pipe m = bind (bind m (n => lift (n + 1))) (n => lift (n * 4))
```

Which is concise but way less clear! We've created a syntax almost as clean as do-syntax, but flexible, because it can support any operators! :D

Could we go entirely point-free? Why yes!

```
foo = .bind (n => n + 1) ~> .bind (n => n * 4)
```

Which in my (biased) opinion, is a lot nicer to read than the Haskell version:

```haskell
(~>) = flip (.)
foo = (>>= \n -> return (n + 1)) ~> (>>= \n -> return (n * 4))
```

And the sugared version we have is only slightly worse than the do-block sugar:

```haskell
foo m = do n <- m
           n <- return (n + 1)
           return (n * 4)
```

Now of course idiomatically in Haskell we'd probably use `fmap`:

```haskell
foo = fmap (*4) . fmap (+1)
```

And we might just combine the actions:

```haskell
foo = fmap (\n -> (n + 1) * 4)
```

But of course we could do this in Llama as well:

```
foo = map (n => (n + 1) * 4)
```

So, no problem! Yers!!!

This does mean, though, that we'll have to ditch unary operators. But that's probably a good thing in many ways, anyway. Support a limited set of operators; encourage written names. FTW.

Last thing: a state monad example. First the Haskell version (this works with the right imports):
```haskell
memoFib n = evalState (go n) mempty where
  go 0 = return 1
  go 1 = return 1
  go n = M.lookup n <$> get >>= \case
    Just result -> return result
    Nothing -> do result <- (+) <$> go (n - 1) <*> go (n - 2)
                  modify $ \tbl -> M.insert n result tbl
                  return result
```

Then a putative Llama version (assume `.then action = .bind _ => action` and `plus n m = n + m`):
```
memoFib (n: Num) = evalState (go n) mempty after {
  rassoc go n = case n of
    0, 1 => 1
    n => get.bind tbl => case lookup n tbl of
      Just result => lift result;
    | Nothing => {
      (map plus $ go n - 1).apply (go n - 2)
      ! .bind result => modify (.insert n result)
      ! .then lift result
    }
}
```

Not *quite* as clean as the Haskell code, but not horrible either. Then again, we don't need monads for this:

```
mut tbl = {d}
memoFib (n: Num) = case n of
  0, 1 => 1
  n => case tbl.lookup n of
      Just result => result
    | Nothing => result after {
      result = memoFib (n - 1) + memoFib (n - 2);
      tbl.insert! n result;
    }
```

So... yeah.
