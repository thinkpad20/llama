trait Monad = 
  lift: a -> Self a
  bind: Self a -> (a -> Self b) -> Self b

trait MonadState = 
  get: Self s
  put: s -> Self ()

object StateT s m a = StateT (s -> m (a, s))

```
> foo = Just 3
> bar (x: m of Monad Num) (m: Num) = x.bind $ n: Num => lift (n + m)
> bar foo 5
Just 8
> baz (x: m of Monad Num, n: Num, m: Num) = 
    x.bind $ k: Num => lift (k * n)
     .bind $ k: Num => lift (k + m)
     

```
