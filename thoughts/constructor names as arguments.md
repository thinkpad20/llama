It would be nice to pass constructor names as arguments. For example, you have a function which takes in a type T, but then depending on what the constructor of T is, it does different things. And as an argument to that function, you can pass in how to respond to which constructor.

```
data Foo = Foo a | Bar a | Baz a 

func (Foo x) = thing1 x
func (Bar x) = thing2 x
func (Baz x) = thing3 x

func foo cName f = case foo.constructor of
  cName -> f foo
  Foo -> thing1 x

```

Blargh, I can't think clearly...
