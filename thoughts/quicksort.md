partition: (a -> Bool) -> [a] -> ([a], [a]) = test -> vec ->
  #|Splits a vector on a boolean condition.
    Elements in the left satisfy; elements in the right don't.|#
  !pass, !fail after
    pass, fail = ref [], ref []
    for x in vec do (if test x then pass else fail) .= push x

quicksort: {Ordered a}. [a] -> [a] = vec ->
  #|Sorts its argument.|#
  return vec unless vec.length > 1
  left, right = partition (< vec 0) vec[1..]
  quicksort left + vec 0 + quicksort right

swap: (r1: Ref a) (r2: Ref a): a =
  #|Swaps the values in two references.|#
  x = r1!; r1 := r2!; r2 := x

trait Sequence a =
  #|Types which can be viewed as a sequence of elements.|#
  next: a -> a, prev: a -> a

_++: {Sequence i}. (r: Ref i): i =
  #|Increments the reference, and returns the value before the increment.|#
  r! before r .= next
