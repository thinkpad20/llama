quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = filter (<x) xs ++ [x] ++ filter (>x) xs

trait Add' a = Add a a a
trait ListLit l = 
  fromlist: List a -> l a
  tolist: l a -> List a
trait Listlike l = ListLit l, Add' (l a)

quicksort : {Listlike l, Ordered a}. l a -> l a =
  [] -> []
  x~xs -> quicksort l + [x] + quicksort r after l, r = partition (< x) xs
