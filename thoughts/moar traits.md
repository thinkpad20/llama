trait Add a b | a b -> c =
  (+) : (a, b) -> c

impl Add a [a] = elem + vec = vec.prepend elem
impl Add [a] a = vec + elem = vec.append elem
impl Add Num Num = n + m = n.add m
impl Add Str Str = s + s' = s.concat s'
impl Add Str Num = s + n = s + n.show
impl Add Num Str = n + s = s + (n + 2).show

addables: [(a|b) of Add a b] = [1, 2, "yellow", "blue"]
mut prev: a of Add a a = Nothing
for i in addables.range
  if i == 0 then prev := addables i
  else 
    print addables i + prev
    prev := addables i
  
for a in addables
  println a + a

2
4
yellowyellow
blueblue

Could introduce *some* ambiguity...
