x: T = foo y

vmap (f: a->b) (v: [a]): [b] = v' after
  v' = mut []
  for x in v do v'.push! (f x)

vmap: (a->b) -> [a] -> [b]

(v: [a]).each (f: a->b) : [b] = vmap f v

each = flip vmap

each: [a] -> (a -> b) -> [b]


lmap (f: a->b) (l: [!a]): [!b] = case l of
  [!] -> l
  x~xs -> f x ~ lmap f xs

lmap : (a -> b) -> [!a] -> [!b]
flip lmap : [!a] -> (a -> b) -> [!b]
each &= flip lmap

each: {[a] -> (a -> b) -> [b], [!a] -> (a -> b) -> [!b]}

assert [1,2,3].each (n => n + 1) == [2,3,4]
assert [! 1,2,3].each (n => n + 1) == [! 2,3,4]

assert_type [1,2,3].each: (Num -> b) -> [b]

# integer literals

123    -> 64-bit int?
123.4  -> Double, 64-bit
123/4  -> Rational number literal?

`u` or `s` postfix for un/signed, then a number denoting number of bits
123s8 : signed, 8 bits
123u32 : unsigned, 32 bits




