# The problem with ambiguous numeric literals

Something as simple as this:

```
println 1/2
```

Problem: `/` could mean any of these!

```
(/): Int -> Int -> Int
(/): Float -> Int -> Int
(/): Int -> Float -> Int
(/): Float -> Float -> Int
(/): Int -> Int -> Float
(/): Float -> Int -> Float
(/): Int -> Float -> Float
(/): Float -> Float -> Float
```

In order to make it tenable, we'd have to do something like:

```
println (1:Int/2:Int : Int) # => 0
```

Pretty poopy. But! Realistically, the types will probably be specified by the surrounding environment. Mostly. Just going with 1 == Int and 1.0 == Float might be easier. OTOH, they could also be other int types, or other float types, etc.

A nicer way to do it might be with traits similar to Haskell. I.e.:

```
trait IntLiteral a = fromint: Int -> a
trait FloatLiteral a = fromfloat: Float -> a
1 : a of IntLiteral
2.4 : a of FloatLiteral
trait Divide a b c = (/): a -> b -> c
1/2 : c of IntLiteral a, IntLiteral b, Divide a b c
```

Would this solve our problem?

```
println (1/2 : Int) # Still ambiguous! Many different satisfying `/`s
```

No :(. It looks this is the exact same issue as with Haskell:

```
Prelude> class Divide a b c | a b -> c where (//) :: a -> b -> c
Prelude> instance Divide Int Int Int where (//) = div
Prelude> 9 // 3

<interactive>:4:1:
    Could not deduce (Divide a0 b0 c)
      arising from the ambiguity check for ‘it’
    from the context (Divide a b c, Num b, Num a)
      bound by the inferred type for ‘it’:
                 (Divide a b c, Num b, Num a) => c
      at <interactive>:4:1-6
    The type variables ‘a0’, ‘b0’ are ambiguous
    When checking that ‘it’
      has the inferred type ‘forall a b c.
                             (Divide a b c, Num b, Num a) =>
                             c’
    Probable cause: the inferred type is ambiguous
Prelude> 9 // 3 :: Int

<interactive>:5:1:
    No instance for (Num a0) arising from the literal ‘9’
    The type variable ‘a0’ is ambiguous
    Note: there are several potential instances:
      instance Num Double -- Defined in ‘GHC.Float’
      instance Num Float -- Defined in ‘GHC.Float’
      instance Integral a => Num (GHC.Real.Ratio a)
        -- Defined in ‘GHC.Real’
      ...plus three others
    In the first argument of ‘(//)’, namely ‘9’
    In the expression: 9 // 3 :: Int
    In an equation for ‘it’: it = 9 // 3 :: Int

<interactive>:5:3:
    No instance for (Divide a0 b0 Int) arising from a use of ‘//’
    The type variables ‘a0’, ‘b0’ are ambiguous
    Note: there is a potential instance available:
      instance Divide Int Int Int -- Defined at <interactive>:3:10
    In the expression: 9 // 3 :: Int
    In an equation for ‘it’: it = 9 // 3 :: Int

<interactive>:5:6:
    No instance for (Num b0) arising from the literal ‘3’
    The type variable ‘b0’ is ambiguous
    Note: there are several potential instances:
      instance Num Double -- Defined in ‘GHC.Float’
      instance Num Float -- Defined in ‘GHC.Float’
      instance Integral a => Num (GHC.Real.Ratio a)
        -- Defined in ‘GHC.Real’
      ...plus three others
    In the second argument of ‘(//)’, namely ‘3’
    In the expression: 9 // 3 :: Int
    In an equation for ‘it’: it = 9 // 3 :: Int
Prelude> 9 // (3::Int) :: Int

<interactive>:6:1:
    No instance for (Num a0) arising from the literal ‘9’
    The type variable ‘a0’ is ambiguous
    Note: there are several potential instances:
      instance Num Double -- Defined in ‘GHC.Float’
      instance Num Float -- Defined in ‘GHC.Float’
      instance Integral a => Num (GHC.Real.Ratio a)
        -- Defined in ‘GHC.Real’
      ...plus three others
    In the first argument of ‘(//)’, namely ‘9’
    In the expression: 9 // (3 :: Int) :: Int
    In an equation for ‘it’: it = 9 // (3 :: Int) :: Int

<interactive>:6:3:
    No instance for (Divide a0 Int Int) arising from a use of ‘//’
    The type variable ‘a0’ is ambiguous
    Note: there is a potential instance available:
      instance Divide Int Int Int -- Defined at <interactive>:3:10
    In the expression: 9 // (3 :: Int) :: Int
    In an equation for ‘it’: it = 9 // (3 :: Int) :: Int
Prelude> (9::Int) // (3::Int) :: Int
3
```

Well, at least we're no *less* expressive than Haskell in this regard.

A relatively straightforward solution is to *default* to Int and Float, but allow this to be overridden. So `foo = 1` would default to `foo` only being type `Int`, but `foo = 1: Float` would allow `Float`, and `foo = 1:{Int, Float}` would allow both.
