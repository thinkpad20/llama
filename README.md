# Llama

Note: really not much to see here yet. I'm just laying out some ideas here. And in truth much
of what's written here is out-of-date and has been replaced by (hopefully) better ideas. On
the other hand, some of these features, or their improvements, are already implemented in the 
existing code, and more to come.

Llama is being implemented, at least initially, in Haskell. If the language gets off the ground,
I might have it compile; possibly to LLVM (which would make its name very appropriate...).

## Quick overview of features

* Static typing plus inferrence. Functions declare their argument types but not their
  return types, which are inferred. Similarly, local variables' types are inferred.

        foo (n: Num, x: Maybe Num) =
          bar = case x of None => 0
                          Some y => y * 2
          n * 8 + bar

* Operators are functions

        (+) : (Str, Char) -> Str
        (s: Str) + (c: Char) = s.append c

* Multiple dispatch: the same name for a function can be reused, provided that 
  1. The argument type is distinct, and
  2. The new declaration is made in the same scope

            (s: Str) + (c: Char) = s.append c
            (c: Char) + (s: Str) = s.prepend c

  3. (2) is not the case if `&=` is used. This distinction prevents definitions from
     earlier scopes creeping unexpectedly into the current one. However, we might
     not need this, and only have function definitions replace existing ones which take
     the same argument type.
     

            foo (n: Num) = n + 1
            bar (n: Num, m: Num) =
              foo (n: Num, s: Str) = s + show (n * 7)
              foo (foo 1, "hello") # error, foo is of type (Num, Str) -> Str

            baz (n: Num, m: Num) =
              foo (n: Num, s: Str) &= s + show (n * 7)
              foo (foo 1, "hello") # OK, definition was extended

* Dot-syntax is a high-precedence reversal of function application.

        foo (s: Str) = s + "!"
        assert foo "hello" == "hello".foo

        bar (s: Str, n: Num) = s + n.show
        assert bar ("hello", 4) == (4, "hello").bar
        assert foo (bar ("hello", 4)) == (4, "hello").bar.foo

* String interpolation. Two forms: `#{}` applies the `show` function to whatever is
  contained inside (making it a type error to include anything which doesn't implement
  this function) and `#[]` does not apply any function (making it a type error to put in
  anything but a string).

       foo = 3
       assert "hello #{foo}" == "hello 3"
       bar = "world"
       assert "hello #{bar}" == "hello \"world\""
       assert "hello #[bar]" == "hello world"

* Variables are default immutable. Distinction between definition (`=`) and assignment 
  (`:=`), with the latter only allowed for mutable variables.

        foo () = 
          result = 0
          for i in range 10
            result := result + i # error, result is immutable
          result

        bar () = 
          result = mut 0
          for i in range 10
            result := result + i # OK
          result

* Move semantics between mutable and immutable variables are important.
* Difference between mutable variables, references, and mutable references.
  - mutable variables can change within their function's scope.
  - references reach outside their current scope and read an earlier value.
  - mutable references can change, so repeated calls to them might not produce
    the same output (similar to IO functions).

  - There are degrees of mutability: for example, a mutable vector can append 
    elements but not change the value at an index. To these ends, there is a 
    clear distinction made between local variables and reference variables. Local
    variables which are captured in a closure become reference variables.
    Mutability is inherited by the reference.
        
        foo (f: a -> b) = 
          count = mut 0
          (x: a) => print "called #{++count} times"
                    f a
        
        bar = foo (n: Num => n + 1)
        a = bar 1 # prints "called 1 times"
        assert a == 2
        b = bar 4 # prints "called 2 times"
        assert b == 5

* convention to put a `!` on the end of mutating functions:

        map! (vec: mut ref [a]) (f: a -> b) = 
          for i in vec.range do vec[: i] := f vec[: i]
  
* `after` keyword lets us declare what we're returning before setting out loops and definitions:

        concat (s: Str) (s': Str) = result after
          result = mut s
          for c in s' do result += c

* Keyword arguments are supported. Still statically typed. Arguments can be declared with a default,
  or else they will appear wrapped in an `Option`.

        range (n: Num, @start=0, @step=1) = result after
          assert step != 0 && if step > 0 then n > start else n < start
          result = mut []
          i = mut start
          comp = if step > 0 then (<) else (>)
          while comp i n
            result.append! i
            i += step
        
        assert 10.range == [0,1,2,3,4,5,6,7,8,9]
        assert range (-5, start=2, step=-2) == [2,0,-2,-4]
        assert (3, start=-1).range = [-1, 0, 1, 2]

        foo (x: Num, @y: Num) = 
          case y of
            None => x
            Some y' => if y' > x then y' else x + 2
 
        assert foo 1 == 1
        assert foo (3, 4) == 4
        assert foo (5, 0) == 7


### Random ideas, mostly out-of-date

* function calls are left-associative:

        foo bar == foo (bar)
        foo bar baz = foo(bar)(baz)
        foo (bar, baz) == foo(bar, baz)
        foo bar, baz == (foo bar, baz)

* dot reference is left associative, higher than function call

        foo.bar == bar foo
        (foo bar).baz == baz(foo(bar))
        foo.bar baz == foo.bar(baz) == bar foo baz == (bar foo) baz
        foo bar.baz == foo (bar.baz) == foo (baz bar)
        foo bar.baz qux = foo(bar.baz)(qux) == foo (baz bar) qux

* this is the parsing we want for tuples... not sure if that's hard tho

        foo (bar, baz).qux == qux(foo(bar, baz))
        foo ((bar, baz).qux) == foo(qux(bar, baz))

* we might have to stick with

        foo (bar, baz).qux == foo(qux(bar, baz))
        (foo (bar, baz)).qux == qux(foo(bar, baz))

* functions are block-structured, declare argument types:

        foo (bar: [a], baz: [a]) =
          blob = bar ++ baz
          if length blob > 3
            print "oh crap"
          	max [head bar, head baz]
          else
          	blob[3]

* unary operators are just functions

        apply1 (f: Num -> Num) = f 10
        assert(apply1 (~) == ~10)

* binary operators are functions which take tuples as arguments

        apply2 = (f: (Num, Num) -> Num) = f (10, 23)
        assert(apply2 (*) == 230)
        (++) = (str1: String, str2: String) =>
          # mut to create mutable version, will make a copy of operand
          result = mut str1
          for char in str2 do result.push! char
          fix result # returns an immutable version of result

* make a mutable version, will come in handy (returns itself)

        append! (str: mut String, str2: String) = str := str ++ str2; str

* functions can be overloaded

        assert (2 * 3 == 6)
        (*) = (str: String, num: Num) =>
          result = mut ""
          for n in [1..num] do result.append! str
          fix result
        assert ("hello" * 3 == "hellohellohello")

* it's a very common pattern to have some mutable accumulator, so
  let's create a syntactic sugar for that, which should also help
  prevent errors when we forget to return said accumulator

        repl = (str: String, count: Num) =>
          with result = "" do for n in [1..count] do result.append! str

* we can use this as any expression

        foo = with result = 0 do result := result + 1
        assert (foo == 1)
        bar = with result = 1 do for i in [1..5] do result.times! i
        assert (bar == 120)
        product = list: [Num] => with result = 1 do for i in list do result.times! i
        assert (bar == product [1..5])

* if we don't want to fix on return:

        repl2 = (str: String, count: Num) =>
          with! result = "" do for n in [1..count] do result.append! str

* that's equivalent to

        repl2 = (str: String, count: Num) =>
          result = mut ""
          for n in [1..count] do result.append! str
          result

* and this means we can mutate it:

        assert ((repl2 "hey" 5).append! "!" == "heyheyheyheyhey!")

* Curried functions are simple

        map (f: a -> b) (vec: [a]) =
          with result = [] do for a in vec do result.push! (f a)

* we might want to make declarations like this

        (str: String) + (c: Char) = with result = str do result.push! c
        assert ("hello" + "!" == "hello!")
* or go crazy and overload it further

        (str: String) + (n: Num) = with result = str do result.append! n.toString
        assert ("catch-" + 22 == "catch-22")

* lambdas are also simple

        add1 = map (x: Num => x + 1)
        addPairs = map (+)
        print(add1 [1..10])
        print(addPairs(zip([1,2,3], [4,5,6]))

* so we can generate curried from uncurried if we want

        curry (f: (a, b) -> c) = a => b => f (a, b)
        assert(1 + 2 == curry (+) 1 2)

* we can also define map as follows:

        map2 = f: a->b => vec: [a] => ...

* or we could define it with pattern matching

        map3 (f: a -> b) (vec: [a]) = case vec of
          [] => [] | a::as => f a :: map3! f as

* or if we want TCO:

        map4 (f: a -> b) (vec: [a]) =
          loop = (acc: [b], []) => acc
               | (acc: [b], (a::as): [a]) => loop (f a::acc, as)
          loop ([], vec)

* we can have a muting map if we want:

        map! (vec: mut [a], f: a -> b) =
          for i in [0..vec.length-1] do vec[i] := f (vec i); vec

* should we be smart enough to figure out that `reverse [1]` is a function
  call on the vector [1], and `vector[1]` is an array index on `vector`?
  * With explicit types, this is possible.
  * Also important to note that if `vec[i]` returns a *copy*, then `vec[i] := 2`
    doesn't have any mutation effect. So `vec[i]` has to give us a reference to
    something which can be updated. Essentially this is what we mean when we say
    `:=`... it's not a function. Or if it is one, it's built-in with this signature:

        (:=) : (mut a, a) -> ()

        slice (v: [a], indices: [Num]) =
          result = mut []
          for n in indices do result.push v[n]
          result

        vector = [0..10].map(x -> x ^ 2)
        assert (vector[0] == 0)
        assert (vector.slice [2] == [4])
        assert (vector.slice [4..7] == [16, 25, 36, 49])

* these symbols can be used to make functions: `+-*/%$|><=.` as long as they don't 
  make up the key symbols.

        (<|) = (func: a -> b, arg: a) => func arg
        (|>) = (arg: a, func: a -> b) => func arg
        assert(foo bar <| baz qux == foo bar (baz qux))
        assert(baz qux |> foo bar == foo bar (baz qux))

* simple types

        struct Point(x: Num, y: Num)
        struct Circle (center: Point, radius: Num)
        struct Rectangle (topLeft: Point, bottomRight: Point)

* pure function

        clone (c: Circle) = Circle(c.center, c.radius)

* overloaded pure function

        (+) = (p1: Point, p2: Point) => Point(p1.x + p2.x, p1.y + p2.y)
        assert(Point(3,4) + Point(9, 1) == Point(12, 5))

* impure function (! by convention)

        move! (c: mut Circle, d: Point) = c.center := c.center + d; c

* overloaded impure function

        move! (c: mut Circle, dx: Num, dy: Num) = c.move!(Point(dx, dy))

* impure, but only because of IO

        report (c: Circle) = print("area is #{c.area}, location is #{c.center}")
        assert(c.report === report) # === is equality by pointer, always works

* optional args get wrapped and appear as option types

        resize! (c: mut Circle, @scale: Num, @newR: Num) =
          case scale of
          	Some scale => c.radius := c.radius * scale
          	Nothing => case newR of
          	  Some r => c.radius := r
          	  Nothing => () # could optionally throw error here

        c = mut Circle(Point(0,0), 0.565)
        c.report() # area is 1.0, location is Point(0, 0)
        c.move!(Point(2, 3)).report() # area is 1.0, location is Point(2, 3)
        c.resize!(scale=4)
        report(move!(c, -3, 2)) # area is 16.0, location is Point(-1, 5)"
        c.resize!(r=3).move!(1, -1) |> report # area is 28.2, location is Point(0, 4)


* traits (type classes)
  - they would allow us to build interfaces and put restrictions on what types
    a function supports.

        trait Mobile =
          move : Self -> Self
          move! : mut Self -> Self

        moveAll! (things: mut [m: Mobile]) = for t in things do t.move!(randN(-10, 10))
        optionOr = (_, Some(thing: a)) => thing | (default: a, _) => default
        doScene (h: Num, w: Num, @seed: Num) =
          r = mut Random <| Time.now() `optionOr` seed
          randPoint!() = Point(rand! r % w, rand! r % h)
          objects = mut [Circle(randPoint!()), 2.5), Rectangle(randPoint!(), randPoint!())]
          scene = buildScene objects
          loop do moveAll! objects


* not sure if we should have higher-order type classes... gets ugly fast.

        trait Functor order 2 =
          map : (Self a, a -> b) -> Self b

        trait MutFunctor order 2 =
          map! : (mut Self a, a -> b) -> ()

        trait Applicative extends Functor =
          pure : a -> Self a
          apply : (Self a, Self (a -> b)) -> Self b

        trait Monad extends Applicative =
          lift : a -> Self a
          bind : (Self a, a -> Self b) -> Self b

        Option as Functor =
          map = (Nothing, _) => Nothing
              | (Some x, f) => Some (f x)

        Option as Applicative =
          pure a = Some a
          apply = (Nothing, _) => Nothing
                | (Some x, Nothing) => Nothing
                | (Some x, Some f) => Some (f x)

        Option as Monad =
          lift a = Some a
          bind = (Nothing, _) => Nothing
               | (Some x, f)  => f x

* What about objects acting like functions? That could be interesting.
  - This introduces the idea of type classes which are parameterized by some
    type (in this case, the argument type with which they're called)

        trait Function t = # of course, the return type is unset...
          call : (self, arg: t) -> u

        [a] as Function Num =
          call (self, n) = self{!n}

        [a] as Function [Num] =
          call (self, indices: [Num]) = self.slice indices

        assert ([1..10] 4 == 5)
        assert ([1..10] [2,5,4] == [3,6,5])

* how about a map?

        [a] as Function (a -> b) =
          call (self, f) = self.map f

        assert (["hello", "world"] allCaps == ["HELLO", "WORLD"])

        Num as Function Num =
          call (self, n) = self * n

        assert (22 3 == 66)
        x = 4; y = 5
        assert (2x + 3y == 23)

        Num as Function (Num -> a) =
          call (self, f) = f self

        replicate times a =
          result = mut []
          for n in [1..times] do result.push! a
          fix result

        twice = replicate 2

        assert(2 twice == [2,2])

        "hello" "world" # Error: `String` doesn't implement `Function String`

* make String an instance of Function:

        String as Function String = call (str1, str2) = str1 ++ str2

* now it will work

        assert ("hello " "world!" == "hello world!")

* one thing we can do is when dealing with immutable vectors, use a
  Clojure-style implementation, but when using mutable vectors, use a
  C-style implementation. Since we'll always know which is which, this
  should be possible and maintain the advantages of each use case. As
  always, `mut` will create a mutable copy of its argument: no immutable
  object can be made mutable, so immutable objects are always safe to use.

        foo = [1,2,3] # foo is clojure-style
        bar = mut [1,2,3] # bar is C-style
        baz = mut foo # baz is C-style, with a copy of foo's contents
        qux = fix bar # qux is Clojure-style, with a copy of foo's contents

* So going from mutable to immutable and vice-versa is a little expensive,
  but keeps things sane. Could make this user-definable with some kind of
  features (immutable/mutable maps...)

        struct List a = End | (::) (value: a, next: List a)

* a `!` after the `[` indicates a linked list, syntactic sugar

        list = [! 1, 2, 3] # linked list, immutable
        assert(list == 1 :: 2 :: 3 :: End)

        list2 = mut list # mutable linked list, deep copied...

* how about a replace function?
        replace! (list: mut [!a]) (toReplace: a, toReplaceWith: a) =
          runner = list # runner grabs a copy of the reference
          while runner != [!]
            if runner.value == toReplace then runner.value := toReplaceWith

        list2.replace!(1, 4)
        assert(list2 == [! 4, 2, 3])

        toList (str: String) =
          mut result = [!]
          for char in str.reverse() do result := char :: result
          fix result

        fello = fix (mut "hello".toList()).replace!('h', 'f')
        assert (fello == "fello")


* How about redoing (.) so it's simply a syntactic sugar for reordering
  its arguments

        inc(n: Num) = n + 1
        assert(1.inc + 2.inc == 5 == inc 0 + inc 3)

* what happens when a mutable object gets left in a closure?

        push! (v: mut [a]) (a: a) = ...

        vec = mut [1,2,3]
        foo = push! vec # type of foo is Num -> (), but it's a side-effecty thing...
        foo 1 # Vec now contains [1,2,3,1]
        2.foo # Vec now contains [1,2,3,1,2]
        vec.push! 4 # now contains [1,2,3,1,2,4]

* ok, so we have `.` as a simple reordering! Interesting.

        inc! = n: mut Num => n := n + 1; n
        k = mut 1
        inc! k # now k = 2
        j = fix k.inc! # now k = 3, j = 3

        map (f: a -> b) (v: [a]) = with result = [] do result.push! f a for a in v
        print <| (x -> x + 1).map [1,2,3,4] # [2,3,4,5]

* overloaded fo funz:

        map (v: [a]) (f: a -> b) = with result = [] do result.push! f a for a in v
        print <| [1,2,3,4].map (x -> x + 1) # [2,3,4,5]

* this might screw with multiple dispatch, though. For example, before we had:

        move! (c: mut Circle, d: Point) = c.center := c.center + d; c
        move! (c: mut Circle, dx: Num, dy: Num) = c.move!(Point(dx, dy))

* and we were able to do `c.move!(1, 2)` and `c.move!(Point(1, 2))`
  but now what if we have:

        move! (c: mut Circle) (d: Point) = c.center := c.center + d; c
        move! (c: mut Circle) (dx: Num, dy: Num) = c.move!(Point(dx, dy))

* then if we say

        c = mut Circle(..)
        c.move! (1, 2)
        c.move! (Point(1,2))

* can we figure that out? I guess we could have c.move! evaluate to a function
  which takes either a Point or two Nums... so we need to be able to formalize
  functions which can take multiple argument types. I think that's doable...

        foo = (str: String) => print "oh ho ho! #{str}"
            | (n: Num) => io.write("foo.txt", "the answer is #{num}")
            | (x: String, y: Num) => foo x; foo (y * 3)
            | (@vec = ["foo", "bar"]: [String]) => print "you entered #{vec}"

* type of multiArgs = (String | Num | (String, Num) | [String]) -> ()
  we just need to ensure that all of the return types are the same. This
  means we can overload operators:

        (-) (a: Num, b: Num) = a.subtract b
        (-) (a: Num) = negate a
        (+) (a: Num) = abs a
        (+) (a: Num, b: Num) = a.add b

* and who knows what else.
  and if later on we overload foo some more, then we will just add to its type
  alternatives as appropriate...

* One issue though is namespacing with structures:

        struct MyStruct = MyStruct(foo: String, bar: Num)
        s = MyStruct("hello", 2)
        print s.foo # prints hello
        foo = k: Num => k ^ 2 * 3
        print s.foo # type error here? no, should be ok
        bar = s: MyStruct => s.bar * 7

* now here's the issue:

        print s.bar # what? is it 2, or 14?

* probably we should make dot syntax always go to a member variable if there,
  and only if not does it make a function call, so the above prints 2.

        print <| bar s # prints 14

* then the following would NOT work:

        print <| foo s # (no function `foo` which takes MyStruct as its argument)

* this should allow us to reuse member variables:

        struct Foo = Foo(bar: String, baz: [Num])
        struct Bar = Bar(baz: Num)
        baz (s: String) = s ++ "!"
        print Foo ("hello", [1,2,3]).baz # [1,2,3]
        print Bar (45).baz # 45
        print "yo".baz # yo!
        print Foo.baz.baz # hello!

* ah, we should probably _not_ make `.` bind higher than application when
  coming after a tuple... so that `foo(bar, baz).qux == qux(foo(bar, baz))`
  and not `foo(qux(bar, baz))`.

* What if we did that, and we DID want `foo(qux(bar, baz))`?
* We'd write `foo((bar, baz).qux)`
* we could play around in parsec until we get this exact behavior...

