## Llama

Llama aims to be a fun and easy to use scripting language which joins key features of functional programming languages with familiar imperative features. The result is a language which is as intuitive to use as Python or Ruby, yet with the expressiveness and (nearly) the safety of languages like Haskell and ML.

### Key features:

#### From the functional side of things:
* Algebraic data types
  - Used to represent several alternative representations of the same type.
  - Llama ADTs expand on those of Haskell and ML by adding in shared variables (fields which are held by all alternatives of a type).
  - Another extension is inheritance. Objects can be extended from other objects. A child can be used in any function which accepts its parent, and not the other way around.
* First class functions
  - Although these are commonplace in most modern languages, they are often awkward to use, either syntactically or semantically, and subsequently powerful concepts like currying and closures see less use than they should. Llama prizes functions as much as any functional language, and employs currying extensively.
* No nulls
  - There is no `null` type or similar, eliminating a large class of errors. The author is of the opinion that it is generally better to fail a lookup than to return `null`, and in cases where returning nothing is an option, a `Maybe` type or similar should be used.
* Gradual type checking with inference
  - As it embraces both functional and imperative paradigms, Llama embraces both static and dynamic typing disciplines, using gradual typing. Write code dynamically or statically as you want. Type-annotated code will be checked at compile time and guaranteed to be type-safe.
* Type classes
  - Called traits in Llama, these are analogous to Haskell's type classes, but with a few extra features, such as the ability to act like Java-style interfaces. They provide a powerful abstraction mechanism and polymorphism in the absence of classes.
* Immutable variables
  - By default, all variables are immutable. This forces the programmer to make explicit which variables can be mutated and in what circumstance, promoting safety and ease of understanding. However, mutable variables are well-supported.

#### From the imperative side:

* Strict evaluation by default
  - Strict evaluation is simple to understand and allows for straightforward and predictable execution, as well as easier debugging and stack traces. Lazy evaluation is optionally available when desired, such as in the evaluation of `&&` and `||`.
* First-class exceptions
  - Exceptions are dealt with much as they are in Python or other imperative languages, allowing for a uniform, intuitive and simple way to deal with failure. The inheritance system provides an easy way to build an expressive exception hierarchy.
* IO
  * No monads are required to perform IO. It is the author's opinion that, in particular with strict evaluation and in a scripting language, monadic IO has more cost than benefit, and is better as an opt-out than an opt-in.
* Mutable variables
  * Llama has a `ref` concept similar to that of SML, allowing mutable structures to be built and allowing the use of loops.
* Loops and imperative control flow
  - Recursion and functional style allows for beautiful expression of certain ideas, but some algorithms are more readily expressed in an imperative style. For-loops, the ability to halt execution early with a `return` or `break`, and a strong exception system provide this.
* Keyword arguments
  - Llama allows a python-style keyword argument system, which reduces the need for wrapper functions and allows defaults to be given easily. Keyword arguments can be type-annotated.

#### Other tidbits:

* String interpolation
* Optionally enforced functional purity
* Clean, light and readable syntax with various nice sugars
* Built-in concurrency

### Let me see it!

I have many examples of Llama code in the `./thoughts/` directory, where I've put most of the musings I've collected regarding the language. However, for completeness, here are a few examples:

Hello world:

```
println 'Hello world'
```

Some factorials

```
fact n = if n < 2 then 1 else n * fact1 (n - 1)
fact = 0, 1 => 1
     | n => n * fact (n - 1)
fact n = result after
  result = mut 1
  for i in n.range do result *= i
```

Declaring objects

```
object Maybe a = 
  Nothing
  Just a
object Either a b <: Maybe b = 
  Left a <: Nothing
  Right b <: Just b

foo = Nothing => 'nummat!'
    | Just a  => a.show
maybe = Just 'foo'
either = Right 'foozle'
assert maybe.foo == either.foo
```

### Current status

Llama is still very much a work in progress. The implementation, such as it is, involves are four main parts, each in various stages of development.

* The parser is in the process of a complete rewrite, but is fairly complete for basic expressions.
* The desugarer is also nearly complete.
* The type checker still needs a few things; object inheritance and type classes are not yet implemented, but the basic type checking algorithm is implemented.
* The evaluator is still very basic, with many expressions not yet evaluated and hardly a thought given to performance. There is a basic REPL implemented.

### Running Llama

Cloning:

```
> git clone https://github.com/thinkpad20/llama
```

Running the REPL:

```
> cd llama/src
> ghc -o llama Main.hs
> ./llama
```

### License

MIT
