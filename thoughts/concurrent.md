## Vague thread/concurrent ideas

```
object Promise a = 
    Working ThreadId 
  | Finished a
spawn : (a -> b) -> a -> Promise b
join : Promise a -> a

mut results={d}
fib (n: Int) =
  if n in results then results n else result after {
    result = case n of 
        0, 1 => 1 
      | n => fib (n - 1) + fib (n - 2);
    results.add! (n, result);
  }

mut promises = map (spawn fib) [1..10]
map (p => print "Result: #{join p}") promises
```
