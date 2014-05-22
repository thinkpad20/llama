Could make kinda/sorta object ideas, where certain functions are "associated" with specific types. They can still be used as first-class functions, but they will be given preferential treatment in resolving ambiguities.

For example:

```
module A
reverse : List a -> List a
reverse list = ...
```

```
module B
reverse : [a] -> [a]
reverse vect = ...
```

```
module C
import A, B
foo = [1,2,3,4,5]
println foo.reverse
```

Here, we have a potential ambiguity: `A` exports a function `reverse`, and so does `B`. However, we know which one we should use: we should use `B`'s version, because it accepts a vector as its argument. Since we know what type `foo` is, this is fine. However, it gets a little dicier in a situation like this:

```
module D
import A, B
foo x = x.reverse
println foo [1,2,3,4,5]
println foo (1~2~3~4~5~End)
```

Here we have `foo: ? -> ?`, because we don't know what type `foo`'s argument is, and we don't know which type of `reverse` to use. With the union system we could narrow this down to `foo: {List a, [a]} -> {List a, [a]}`. More precisely, we could narrow it to `foo: {List a -> List a, [a] -> [a]}`. This might prevent a class of errors (for example, it would prevent calling `foo 2` or `x: Int = foo [1,2,3]`). But either way, we don't know until `foo` is called, what it will return.

What we want is the ability to be able to determine at runtime what function we should call. Both `List` and `[]` have versions of `reverse`. If `reverse` is put into a trait, then we already have machinery to choose the correct version. But what if we haven't put it into a trait? We probably don't want to automatically make traits. But it might not be a bad idea if we restrict it to the first argument. In other words:

> When a function *f* is declared with a name *n* and type *a -> ...*, add an entry for that function into that function's prototype, so that `a.prototype.n = f`.

```
foo (x: Int) = x * 7
foo (x: Str) = x.reverse
```

```javascript
Int.prototype.foo = function() { return this * 7; }
Str.prototype.foo = function() { return this.reverse; }
function foo (x) {
  if (x.isType(Int) || x.isType(Str)) {
    return x.foo;
  } else {
    throw new TypeError('Blah blah');
  }
}
```

The problem with this approach is that it's not as expandable. We'd probably want to do something like:

```javascript
var foo$implementations = {
  Int: function(x) { return x * 7; }
  Str: function(x) { return x.reverse; }
}
function foo (x) {
  if (x.type in foo$implementations) {
    return foo$implementations[x.type](x);
  } else {
    throw new TypeError('Blah blah');
  }
}
```

This ends up just being a weaker version of the `trait` idea, which is kind of what it was already. Then we would just need to augment the `import` logic so that it would combine any overlapping dictionaries. We'd only need to throw an error if there were two overlapping implementations for the *same type*.

In the end it kind of ends up being a multiple-dispatch idea. The problem here is that *every* function is being dealt with in this way, which gives unnecessary overhead for functions which are not overloaded. So we might instead want to only create these function if there are namespace clashes between two modules; otherwise functions can stay as-is. Also it's worth noting that if we can statically guarantee that a variable is of a certain type, then we could replace the overloaded call with the "actual" call at compile-time, so it might be OK.

Returning to our original example, then:

```
# module A
function(imports, exports) {
  exports.reverse = function(list) {
    ...
  }
}(..., this);
```

```
# module B
function(imports, exports) {
  exports.reverse = function(vect) {
    ...
  }
}(..., this);
```

```
# module C
$lc = require('llama-core');
function($imports, exports) {
  $lc.import($imports, 'A'); // Adds `reverse` to imports
  $lc.import($imports, 'B'); // Sees overlap of `reverse`, merges
  var foo = $lc.Vector(1,2,3,4,5);
  $lc.println(reverse(foo));
}(..., this);
```

So we could be really clever and detect when (a) two functions with the same name are declared within the same module, and when (b) two functions with the same name are declared in separate modules which are subsequently imported.

**Remaining question**: what if the two functions share their first *k* arguments? E.g.:

```
replicate : Int -> [a] -> [a]
replicate n vec = result after result = mut []; for n.range do result += vec

replicate : Int -> List a -> List a
replicate n list = replicate n list.to_vec |> to_list
```

In this case, we'd have to employ tricks similar to what we have with our traits.

But then, what would the type of `replicate` on its own be? Or even the type of `replicate 2`? We'd no longer be able to file it under a trait name. Perhaps instead, we could use multifunctions; say `replicate: {Int -> [a] -> [a], Int -> List a -> List a}`, `replicate 2: {[a] -> [a], List a -> List a}`. This could get hairy quickly, though... But it might be fine.

**Another question**: What if we have two overlapping functions that don't declare their types?

```
foo x = x + 3
foo y = y.reverse
```

In this case, we should probably just throw an error, or else override the previous definition.

**Yet another question**: What about two functions that only differ in their return types?

```
foo : Int -> Int
foo x = x + 3
foo : Int -> Str
foo x = '!!' * x
```

In this case, should we throw an error? Or create a stub, like with traits? In fact, come to think of it, does this discussion completely obviate traits, and make them just nice ways of putting constraints on types? The *problems* with this approach are: 

(1) Traits a very elegant. 
(2) multifunctions are not. In fact, they might not even be sound. They certainly don't seem to play well with type inferrence and parametric polymorphism, but we don't know yet if that's something that can be worked around.
(3) making explicit what functions are overloaded is arguably a good thing.

The advantages:

(1) Traits are clunky.
(2) Multifunctions are clunky to type, but simple to use.
(3) Making explicit what functions are overloaded is only possible when we know *ahead of time* what will be overloaded. Considering that in the general case we don't know that ahead of time, it makes more sense to make it ad-hoc.
