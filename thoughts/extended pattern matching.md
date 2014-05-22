## Getting rid of `|`

We'd potentially like to free up `|` to be a function instead of a reserved symbol. We could use `;` to delimit case alternatives, object constructors, etc. Of course, that brings up an issue with the existing use of `;` to delimit expressions in blocks. However, we could simply allow blocks to be wrapped in parentheses:

```
# Has side-effect at load time of printing 3, but has the value 5.
foo = (println 3; 5)
```

Is this equivalent to `foo = println 3; 5`? Or would that simply assign `foo` to `()` and return 5? That sounds like a better default in some ways. Keep the blocks explicit, except when default (e.g. on the right side of an arrow). 

We don't need parentheses in this case:
```
case x of 
  Y z -> foo x; z
  Z a b -> println a + b; a * b + 45
```

But if we're set on inlining everything, we can use parentheses.
```
case x of Y z -> (foo x; z); Z a b -> (println a + b; a * b + 45)
```

As per our earlier idea of first-class blocks, this is similarly useful:

```
# block that takes an argument
foo = block n (println n; return 5)

# block that takes no argument: this is simply a deferred `break`
bar = block break

# function that uses these blocks
qux n = 3 after
  for x in Random\range 10
    if x > 5 then foo x    # Will return early (returning 5)
    if x == 5 then bar     # Will break out of this loop
println qux 10
```

Here blocks can be viewed as similar to lambda expressions; however, control flow statements like `break`, `return` and `continue` apply to the scope *in which they are called*, not the scope of the block. A matter of debate is whether blocks should be able to be passed or returned as values. Regardless, first-class blocks are a somewhat nebulous feature and need not be fully fleshed out now. The semantics of the more normal form of blocks is clear, and only the syntax need be considered. It's worth noting that blocks seem to be almost identical to macros, and simply embracing the idea of macros in a Lisp-y way might be better than a somewhat half-baked block idea.

Having `|` as a function would be great for shell scripting:

```
#!/usr/bin/env llama
import Bash\cat, \grep
cat 'my_file.txt' | grep ['-v', 'foobar'] | println
```

Of course, using `|>` or `!` instead of `|` isn't that bad... But still, it's nice to get rid of a reserved symbol if we don't need it, which it seems we don't.

## Cool things we can do with pattern destructuring

```
> object Foo = Foo Int; Bar Str Int with foo: Int
> foo = Bar 'hello' 6
> println foo\foo
0
> foo' = foo with foo=10
> println foo'\foo
10
> println foo'\0
hello
> println foo'\1
6
> println foo\2
DerefError: object `Bar 'hello' 6 with foo=0` only contains 2 values, not 3.
In file '<stdin>', line 1, column 9
In expression `foo\2`
> case foo of Bar s => println s
hello
> case foo of Bar a b c => println a
DerefError: object `Bar 'hello' 6 with foo=0` only contains 2 values, not 3.
In file '<stdin>', line 1, column 9
In pattern: Bar a b c
In expression: case foo of Bar a b c => println a
> object X = X Int; Y Str; Z Str Int
> f x = case x of _ a => println a; Z s i => println s.append i.show
> f (X 1)
1
> f (Y 'hello')
hello
> f (Z 'yo' 3)
yo3
```

That's cool; it means that we can pattern match on the number of values, independent of which constructor is used.

We could do all kinds of cool things like iterate through values, get a list of values, etc...

```
list_values : ? -> List ?
list_values x = 
  result = mut Empty
  for i = mut 0; ; i.incr!
    try result .= prepend x\i
    catch _ do return result
```

This is some really cool stuff. Dynamic typing at first, then gradual? Sounds good to me! :)
