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
      x = 2

  `x` is a stub because it could be anything which can be written as an int
  literal. So it could be an int, a float, a nat, etc. So the representation of
  `x` is Stub (t -> fromint 1 : t)
  `show` is a stub because its representation depends on a type.
  `show: Int -> Str` is different than
  In either case, we can extract values from PolyValues using `:`.
      assert (f: Int == 1)
      assert (f f == 2)
      assert (x: Int == 2)
      assert (f x == 3)
  |#
  ValueSet {Type=>Value}
  PolyFunc (Type -> Value -> Value)
  Stub (Type -> Value)

x = 1 # x is the numeral 1, which is something which implements Intlit
      # so x = {1: Int, 1.0: Float}
