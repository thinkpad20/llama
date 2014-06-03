We can actually be very straightforward with how we translate our `with` expressions to JavaScript:

```
type Foo with a=1, b=2
println 'Foo type has been defined.'
foo1 = Foo
foo2 = foo1 with a=10, b=11
```

```javascript
var Foo$Type = new LlamaType(['Foo'], {a: $int(1), b: $int(2)});
var Foo = new LlamaObject(Foo$Type, 'Foo');
println($str('Foo type has been defined.'));
var foo1 = Foo;
var foo2 = foo1.with({a: $int(10), b: $int(11)});
```

The first line in the JavaScript creates a type which has one constructor (`Foo`), and two attributes, defaulted to `1` and `2`. The second line is created at the same time, which is the constructor for the type (there's only one). It's a constant since it doesn't take any arguments, and since it stores its type, it can use it to initialize its variables. In the fourth line, we instantiate a foo -- this is just the same as assigning it to a variable, because it's a constant, so there's no need for `new`. In the fifth line, we are modifying its variables, so we use the `.with` method. This will create a new object with the updated values.

What about refs, though?

```
type Bar with a=ref 1, b=ref 2
```

The issue, here, is that since we have a singleton `Bar` object, those `ref`s are subject to change! They won't always be `ref 1` and `ref 2`. For example:

```
bar1 = Bar
println bar1.a # prints 'ref 1'
bar1.a := 10
bar2 = Bar
println bar2.a # prints 'ref 10'
```

This is just something that is going to have to be dealt with! As in "don't do that unless you know what you're doing." After all, we can always simulate the OOP-y style:

```
new (): Bar = Bar with a=ref 1, b=ref 2
bar1 = new(): Bar
println bar1.a # prints 'ref 1'
bar1.a := 10
bar2 = new(): Bar
println bar2.a # prints 'ref 1'
```
