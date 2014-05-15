collatz (n: Int): List Int = go n after
  cache: {Int => List Int} = {}
  go 1 = lift 1
  go n if n in cache = cache n
  go n = res after
    res = n ~ go (if n.even then n/2 else n*3 + 1)
    cache.add! n res

trait Call a b with __call__: Self -> a -> b
implement Call a b for (a -> b) with __call__ = ($)
implement Call Int a for [a] with __call__ = unsafe_read
implement Call a b for {a => b} with __call__ = unsafe_get
