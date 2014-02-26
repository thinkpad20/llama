trait Ord =
  compare: Self -> Self -> Ordering

(a of Ord) > (b of Ord) = a.compare b == Greater
(a of Ord) >= (b of Ord) = a.compare b in [Greater, Equal]
(a of Ord) < (b of Ord) = a.compare b == LessThan
(a of Ord) <= (b of Ord) = a.compare b in [LessThan, Equal]
(a of Ord) == (b of Ord) = a.compare b == Equal
(a of Ord) != (b of Ord) = a.compare b != Equal

merge (v1: [a of Ord]) (v2: [a of Ord]) = result after
  result = mut []
  i, j = 0, 0
  loop
    if i >= v1.length then break after result.append! v2.slice j
    if j >= v2.length then break after result.append! v1.slice i
    if v1 i < v2 j then result.push! (v1 i++)
    else result.push! (v2 j++)

sort (v: [a of Ord]) = case v of
  [] or [x] => v
  xs => (sort v.firstHalf).merge (sort v.secondHalf)

parseInt (s: Str, @base=10) = n after
  n = mut 0
  for c in s
    d =
      if c >= '0' && c <= '9' then c - '0'
      else if c >= 'A' && c <= 'Z' then c - 'A' + 10
      else if c >= 'a' && c <= 'z' then c - 'a' + 10
      else throw Exception "Char #{c} out of range"
    if base <= d then throw Exception "Overflow"
    n := n * base + d

-- where it is and isn't acceptable to put a return:
(-): (Char, Char) -> Int

foo (n: Num) =
  bar = case n of
    1 => return 5 -- illegal? Or is it ok as long as it matches return type?
    n => n * 7
    -- Note: if alternatives not exhaustive might throw error
  case foo * bar of
    p if p > 20 => println "Hey there #{p}"
                   return 8 # no problem
    # no need for exhaustive alternatives
  -- exceptions can go anywhere
  throw Exception "Expected to have returned by now"

object OperationType =
  Addition
  Subtraction
  Multiplication

object Exception = Exception (message="")
object OverFlowException : Exception =
  OverFlowException (opType: OperationType)

-- need to thinking about how to extend objects with multiple
-- alternative constructors. Possibly... you can't?

bar = n: Num => q after
  try do f = foo n
  catch e => case e of
    -- here's where OO with inheritance becomes very useful!
    -- Is there a reason we shouldn't support that?
    -- go with the "first match", meaning that the most specific
    -- should go first
    NoProblemException(...) =>
      println "Hey, there was an exception but it's no problem! :D"
    OverFlowException(...) =>
      println "hey, we overflowed. returning -1"
      return -1
    Exception(..., message => msg) =>
      println "Got an exception with the message #{msg}!"
      Sys/exit 1
    -- an inexhaustive pattern here will simply mean exception isn't caught
  q = mut f
  for i in 10.range
    q *= (i * 19)