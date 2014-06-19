Interesting idea: type classes as modules.

A type class declaration creates a context. We can create functions inside of it which are entirely dependent on (parameterized by) the context. Since functions can take default implementations, this is pretty much similar to class methods which use the methods of an object (and these are hidden from the user). These methods could even be hidden from the user, or fixed (in terms of implementation).
