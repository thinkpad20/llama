Destructuring definitions

```
λ foo = Just 2
λ (bar, baz) = (foo, foo)
λ Just qux = baz
λ bux = [baz, Just 4]
λ [Just bix, ...] = bux
λ assert qux == 2
```

```
object Error = Error (@message: Str)
object BoundsError extends Error
  = Error ("Index out of bounds") 
    ==> BoundsError (index: Int)

foo = [1,2,3]
try
  println foo 3
catch BoundsError index as e
  println "Message was #{e..message}"
  println "WTF is wrong with you, with #{index}?"
  4
catch Error message
  println "Whoa, what's this? #[message]"
  println Sys..get_stack_trace()
  throw

show (e: Error) = "Exception: #[e::message]"
show (be: BoundsError) = be..super.show + "(#{be..index})"

err = BoundsError 3
assert err.message = "Index out of bounds"

foo n = if n < 0 then raise ValueError

object Foo = Foo (index: Int)
index (foo: Foo) = foo.index - 1
windex foo = foo.index + 2
blistex (foo: Foo) = foo.index + 2

index (n: Int) &= n * 3
dindex foo = foo.index * 5 # error, `index` is ambiguous

foo = Foo 1
assert foo.index == 1 # member variable takes precedence
assert index foo == 0 # function application
assert foo.windex == windex foo == 2
assert foo.blistex == blistex foo == 3
```

```
object Foo = Foo (i: Int) 
           | Bar (s: Str)

f (foo: Foo) = case foo of
    Foo k => k + 1
  | Bar s => s.length

# Using root-level objects
a = Bar "wazzap"
b = Foo 3
assert f a == 6
assert f b == 4

# Create an object which extends it
object Baz extends Foo = Buzz i (j: Int) extends Foo i
                       | Bizz (k: Int) extends Bar "hello"
                       | Blooz extends Foo 6

# Using inherited objects
assert f (Buzz 1 2) == 2 # because it contains `Foo 1`
assert f (Bizz 100) == 5 # because it contains `Bar "hello"`
assert f Blooz = 7       # because it contains `Foo 6`

# With member variables not in constructors
object Blorp = Blorp (i: Int)
             | Bloop (i: Int) (s: Str) {
                self.bloop = s + "!"
                self.blorp = 10 i + s.length
             }
             | Blap
  sharing bloop: Str # must be specified in constructor
          blorp: Int = 10 # defaults to 10

g (b: Blorp) = 
  println "b says #{b.bloop}!"
  case b of
    Blorp i => return i * 7 + b.blorp
  | Bloop _ _ => return b.blorp
  | Blap => raise Error "encountered a Blap with blorp = #{b.blorp}"

blip = Blorp 5 with bloop = "hey there"
assert blip.bloop == "hey there"
assert blip.blorp == 10
assert g blip == 45

blop = Blap with {bloop = "yo", blorp = 2}
assert blop.bloop == "yo"
assert blop.blorp == 2
try do assert g blop == 6
catch Error _ do println "Error as expected"

blup = Bloop 10 "hey"
assert blup.bloop == "hey!"
assert blup.blorp == 103
assert g blup == 103
```
