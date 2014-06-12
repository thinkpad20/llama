typedef Name = String
type Value =
  Float Float
  Int Int
  String String
  Bool Bool
  Ref (Ref Value)
  Object (items: [Value]) (attributes: {Name=>Value})
  with _type: Type

type PolyValue =
  #|
  ValueSets are sets of actual values, indexed by their types. A stub is like
  a closure; it's a capture of a value which has the potential to become a
  value.
  An example of a ValueSet:
      f: Int = 1
      f: Int -> Int = (+ 1)
  Here `f` contains the values 1 and (x -> x + 1).
  Examples of Stubs:
      x = 1
      show
  `x` is a stub because it could be anything which can be written as an int
  literal. So it could be an int, a float, a nat, etc.
  In either case, we can extract values from PolyValues using `:`.
  |#
  ValueSet {Value}
  Stub (Value -> Value) Value

x = 1 # x is the numeral 1, which is something which implements Intlit
      # so x = {1: Int, 1.0: Float}
