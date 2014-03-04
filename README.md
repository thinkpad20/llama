# Llama

A functional language for imperative programmers. An imperative language for functional programmers. A language that does what you want it to. A language which values safety and expressivity but not at the cost of pragmatism and ease of use.

## Overview of some features

* Strong support for functional programming, with first-class functions, algebraic data types and various forms of polymorphism. However, there is also strong support for imperative programming, with loops, mutable variables, objects and the like.

* *Optionally* enforced functional purity. The compiler can detect which functions are *pure* (meaning, having no effects outside of their own scope), which are *local* (side-effects limited to only affecting their arguments), and which are *impure* (mutating captured variables or performing IO). Function declarations can be optionally annotated as `pure` or `local` which will cause a compile-time error if the annotated functions do not meet their restriction.

* Static typing plus inferrence.  The type system is a modified version of Hindley-Milner which forgoes full inferrence as a goal in order to provide multiple dispatch (see below) and other features. Functions declare their argument types but not their return types, which are inferred. Similarly, local variables' types are inferred.

        foo (n: Num, x: Maybe Num) =
          bar = case x of Nothing => 0
                          Just y => y * 2
          n * 8 + bar

* Operators are functions. Binary operators take a tuple as an argument.

        (+) : (Str, Char) -> Str
        (s: Str) + (c: Char) = s.append c

* Every expression has a return value. Blocks, or lists of expressions, will return whatever the last expression in the block is. Although an imperative language, there is no concept of a "statement", only expressions. This means that `for` and `while` loops must return something as well, so they return a `Maybe` type: completing the loop or breaking without a value returns a `Nothing`, and breaking with a value returns a `Just`. Similarly, `if`s without a corresponding `else` will return `Nothing` if their condition evaluates to `False` and otherwise `Just` wrapped around the result of their "if true" branch.

        foo (v: [Str], targets: [Str]) = 
          # result of this for loop is irrelevant
          for s in strings
            # this for loop has a meaningful return value
            index = for (x, i) in v.withIndices do if x == s then break i
            case index of
              Nothing => println "#[s] wasn't found"
              Just i  => println "#[s] found at index #{i}"

* Multiple dispatch: the same name for a function can be reused, and different behavior can be specified by different types. For example, adding two vectors is different than adding two numbers, and calling "reverse" on a list is different than on a map. Because binary operators take tuples as arguments, any *pair* of operands to a binary operator can specify a different behavior set, so that for example:

        (s: Str) + (c: Char) = s.append c
        (c: Char) + (s: Str) = s.prepend c
        (s1: Str) + (s2: Str) = s1.concat s2

* In most cases, dot-syntax is a high-precedence reversal of function application.

        foo (s: Str) = s + "!"
        assert foo "hello" == "hello".foo

        bar (s: Str, n: Num) = s + n.show
        assert bar ("hello", 4) == (4, "hello").bar
        assert foo (bar ("hello", 4)) == (4, "hello").bar.foo

  - However, for types with named member variables, the `.` will overload to dereference that field of the object.

        ```
        object Point = Point (x: Num) (y: Num)
        (p1: Point) + (p2: Point) = Point (p1.x + p2.x) (p1.y + p2.y)
        p = Point 3 4
        q = Point 5 8
        println (p + q).y
        ```

  - This means that member variables don't pollute the global namespace.
        
        ```
        object Point3 = Point3 (x: Num) (y: Num) (z: Num)
        (p2: Point) + (p3: Point3) = Point3 (p2.x + p3.x) (p2.y + p3.y) p3.z
        ```

* Objects can be treated as functions, even if they are not. For example, a vector or map "operating on" a key/index will lookup the key/index. A string "operating on" another string will concatenate them. A number "operating on" another number will multiply it.
        
        vec = [1..10]
        assert vec 3 == 2

        dict = {"hello" => "world", "hi" => "there"}
        assert dict "hello" == "world"

        strA = "hello"; strB = " world!"
        assert strA strB == "hello world!"

        a = 3; b = 2
        assert 2a + 6b == 18

  - Just as with any function, we can specify different behavior based on argument type, so we can have
  
        ```
        # vector operating on another vector
        assert [1,2,3] [4,5,6] == [1,2,3,4,5,6]

        # vector operating on a number
        assert ["hey","there"] 1 == "there"
        ```

  - This might seem a little dangerous, as one can mistakenly "call" an object with another object. But with static typing, the great majority of these errors should be caught early.

* String interpolation. Two forms: `#{}` applies the `show` function to whatever is contained inside (making it a type error to include anything which doesn't implement this function) and `#[]` does not apply any function (making it a type error to put in anything but a string).
      
        foo = 3
        assert "hello #{foo}" == "hello 3"
        bar = "world"
        assert "hello #{bar}" == "hello \"world\""
        assert "hello #[bar]" == "hello world"

* Variables are default immutable. Distinction between definition (`=`) and assignment (`:=`), with the latter only allowed for mutable variables.

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

* Operators ending in `=` except for `==`, `>=` `<=` `!=` are syntactic sugar:
        
        a = mut 0
        b = mut 0
        assert (a := a + 1) == (b += 1)

        # some custom symbol
        (a: Num) *$! (b: Num) = a * b + 7
        c = mut 8
        c *$!= 3
        assert c == 31

        # this even works for function application!
        s = mut "hello"
        s .= reverse
        assert s == "olleh"
        s .= sub 2
        assert s == "leh"

* The `after` keyword lets us declare what we're returning before setting out loops and definitions:

        concat (s: Str) (s': Str) = result after
          result = mut s
          for c in s' do result += c

        fact (n: mut Num) = n after for m in range (n, start=1) do n *= m

        join (strs: [Str], @by="") =
          case strs of 
            [] => ""
            [s] => s
            strs => result after
              result = mut strs 0
              for str in strs.sub 1 do result += by + str

* there is also a `before` keyword, when we want to store some result before a side-effect occurs:
        
        shift! (s: ref Str) = s[0] before s .= sub 1
        s = mut "hello"
        assert s.shift! == 'h'
        assert s == "ello"

* Unary operators are also functions. Prefix and postfix are distinct; when passing unary operators as arguments we use a `_` to indicate whether it is postfix or prefix.
            
            ++ (n: ref Num) = n += 1
            (n: ref Num) ++ = n before ++n
            foo = (mut! [1..10]).map! (++_)
            assert foo == [2..11]

  - Note in the above example the `mut!` designation; this creates a *deeply mutable* object which allows not only the reference, but the things to which the reference refers, to be mutated.

* Move semantics between mutable and immutable variables are important.
* Difference between mutable variables, references, and mutable references.
  - mutable variables can change within their function's scope.
  - references reach outside their current scope and read an earlier value.
  - mutable references can change, so repeated calls to them might not produce
    the same output (similar to IO functions).

  - There are degrees of mutability: for example, a mutable vector can append 
    elements but not change the value at an index. To these ends, there is a 
    clear distinction made between local variables and reference variables. Local variables which are captured in a closure become reference variables.
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

* There is a convention to put a `!` on the end of mutating functions:

        each! (vec: ref [a]) (f: a -> b) = vec after
          for i in vec.range do vec[: i] .= f
        vec = mut [1..5]
        vec.each! (n: Num => n % 2)
        assert vec == [1,0,1,0]

* Keyword arguments are supported, and are statically typed. Arguments can be declared with a default, or else they will appear wrapped in a `Maybe`.

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
            Nothing => x
            Just y' => if y' > x then y' else x + 2
 
        assert foo 1 == 1
        assert foo (3, 4) == 4
        assert foo (5, 0) == 7

## Current Status

Llama is mostly a set of ideas now, but some of this stuff has been implemented. The implementation language is Haskell, although this might change.

At the current stage the parser is the most complete; I would guess about 80% of the proposed language is being parsed, but many features remain there such as user-specified operator precedence, wildcard characters, indentation-based blocks, traits and objects, etc. The type checker is less complete; we still lack a solid way to type overloaded functions, and there's nothing yet on ADTs, accessors, objects-as-functions, and much more. For the evaluator, we have a (very) basic REPL but very little works yet beyond the basics. Also, performance is quite lacking due to the implementation (I'll probably need to switch to IO monad-based types for evaluator state). 

However, development is active and hopefully, a version 0.0.1 will be done in the next couple of months. I'm currently focused on building an interpreted implementation, but if the language gets off the ground, I will likely have it compile, possibly to LLVM (which would make its name very appropriate...). Compiling to JavaScript is also a very real possibility; after all, I already have one language (Kirei) which does this, so I have a ready-made JavaScript code generator to use.

 Obviously I welcome any kind of contributions, though I doubt anyone will be interested until (I hope) things get more complete.
