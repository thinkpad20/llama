One issue with creating implicit traits for everything is that we don't know ahead of time what functions are going to be declared, so we don't know which way to expand a function. For example, let's say we have this guy:

```
from_number (i: Int): Str = ...
```

Well in our system up until now, we'd create something like this:

```
trait FromNumber with from_number: Self -> Str
implement FromNumber for Int with from_number = ...
```

But we can see from the name of the function that it's going to vary in its *return type* more than its argument type. For example, we might want to make `Bool`s out of numbers. So what we really want is:

```
trait FromNumber with from_number: Int -> Self
implement FromNumber for Str with from_number = ...
implement FromNumber for Bool with from_number = ...
```

But, there are multiple types of numbers!

```
from_number (i: Int): Str = ...
from_number (f: Float): Str = ...
from_number (i: Int): Bool = ...
from_number (f: Float): Str = ...
from_number (i: Int): Float = ...
from_number (f: Float): Int = ...
```

Well, shit. So do we need a multiparam trait for that then?

```
trait FromNumber a b with from_number: a -> b
implement FromNumber for Int Str with from_number = ...
implement FromNumber for Float Str with from_number = ...
implement FromNumber for Int Bool with from_number = ...
implement FromNumber for Float Bool with from_number = ...
...
```

The implementation starts to get pretty hairy too, and probably less efficient as well.

Of course, we're assuming that all of these functions are typed, as well. Untyped functions, I think, we can safely ignore.

A few options:

* Nix the automatic polymorphism. Simplest but lame.
* Restrict automatic polymorphism to polymorphism in the *argument* types. Similar to normal OOP-y imperative stuff.
* Figure out how to create everything on the fly, and damn the overhead.
* Allow the user to specify that a function is overloaded in either the argument or return type, or both. Or, make the default opt-in and allow them to opt-out.
