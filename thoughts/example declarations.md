
foo x:Int y:Int : Int = case x of
  y -> y + 1
  _ -> x - 1

with Num a
foo x:List(a) y:a : a = case x of
  [] -> 0
  [a, `y, ...] -> y + a
  [0, rest...] -> 23 * sum rest
  _ -> sum x

foo: List a -> a -> a with Num a = 
  [] -> 0
  [a, y, ...] -> y + a
  [0, rest...] -> 23 * sum rest
  x -> sum x

foo x:List(a) y:a : a with Num a = case x of
  [] -> 0
  [some a, y, ...] -> y + a
  [0, rest...] -> 23 * sum rest
  _ -> sum x
