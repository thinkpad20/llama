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
  - There is no `null` type or similar, eliminating a large class of errors. The author is of the opinion that most of the cases where `null` is used in most code, it is generally better to either throw an exception, or use an option type (`Maybe` in Llama).
* Gradual type checking with inference
  - As it embraces both functional and imperative paradigms, Llama embraces both static and dynamic typing disciplines, using gradual typing. Write code dynamically or statically as you want. Type-annotated code will be checked at compile time and guaranteed to be type-safe.
* Traits
  - Analogous to Haskell's type classes, but with a few extra features, such as the ability to act like Java-style interfaces. They provide a powerful abstraction mechanism and polymorphism in the absence of classes.
* Immutable variables
  - By default, all variables are immutable. This forces the programmer to make explicit which variables can be mutated and in what circumstance, promoting safety and ease of understanding. However, mutable variables are well-supported (see below).

#### From the imperative side:

* Strict evaluation by default
  - Strict evaluation is simple to understand and allows for straightforward and predictable execution, as well as easier debugging and stack traces. Lazy evaluation is optionally available when desired, such as in the evaluation of `&&` and `||`.
* First-class exceptions
  - Exceptions are dealt with much as they are in Python or other imperative languages, allowing for a uniform, intuitive and simple way to deal with failure. The inheritance system provides an easy way to build an expressive exception hierarchy.
* IO
  * No monads are required to perform IO. It is the author's opinion that, in particular with strict evaluation and in a scripting language, monadic IO has more cost than benefit, and is better as an opt-out than an opt-in.
* Mutable variables
  * Llama has a `ref` concept similar to that of Standard ML, allowing efficient mutable structures to be built and allowing the use of loops.
* Loops and imperative control flow
  - Recursion and functional style allows for beautiful expression of certain ideas, but some algorithms are more readily expressed in an imperative style. For-loops, the ability to halt execution early with a `return` or `break`, and a strong exception system provide this.

#### Other tidbits:

* String interpolation à la CoffeeScript
* Useful data structure primitives, all with mutable and immutable versions
  - vectors
  - maps
  - sets
  - JSON blobs
* Keyword arguments (type safe!)
* Clean, light, expressive and readable syntax
* Built-in concurrency (planned)
* Optionally enforced functional purity (planned)

### Let me see it!

I have many examples of Llama code in the `./thoughts/` directory, where I've put most of the musings I've collected regarding the language. However, for completeness, here are a few examples:

##### Hello world:

```
println 'Hello world'
```

##### Some factorials

```
fact n = if n < 2 then 1 else n * fact1 (n - 1)
fact = 0, 1 -> 1
     | n    -> n * fact (n - 1)
fact n = !result after
  result = ref 1
  for i in n.range do result *= i + 1
```

##### Declaring objects

```
type Maybe a = Nothing; Just a
type Either a b <: Maybe b = 
  Left a <: Nothing
  Right b <: Just b

foo = Nothing -> 'nummat!'
    | Just a  -> a.show
maybe = Just 'foozle'
either = Right 'foozle'
assert maybe.foo == either.foo
```

##### ADTs with shared attributes

```
type Token = 
  Word Str
  Punc Str
  NewParagraph
  with line: Int; col: Int

show_token = Word w -> 'Word #{w}'
           | Punc s -> s
           | NewParagraph -> 'New Paragraph (#{@\line}, #{@\column})'

tokenize (input: Str): List Token = ...
toks = tokenize 'This\nis llama!!!'
assert toks 0 == Word 'This' with line=1; col=1
assert (toks 1)\line == 2 && (toks 2)\column == 4
assert toks 3 == Punc '!' with line=2; col=9
```

##### Keyword args

```
with_file (loc: FilePath; mode='r') (f: File -> a): a =
  file = open (loc, mode)
  f file before file.close

save (file: File) (; location: Str) = 
  save_to = case location of Nothing -> file\source; Just loc -> loc
  with_file (save_to, 'w') (.write file\contents)

with_file 'foo.txt' $ f ->
  f.append 'hey there!'
  f.save 'bar.txt'
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

Running the REPL: (NOTE: currently broken)

```
> cd llama/src
> ghc -o llama Main.hs
> ./llama
```

### License

MIT
