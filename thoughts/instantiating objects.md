> object Foo = Foo Str | Bar | Baz Int attrs size : Int
> foo = Foo 'hello'
ObjectError: object `Foo` requires attribute `size` (`with size=...`)
> foo = Foo 'hello' with size=10
> println foo
Foo 'hello' with size=10
> my_func f = f\size
Exception
  In the expression `my_func f = f\size`
  In the expression `f\size`
  TypeError: can't determine the type of `f` to know if it has the attribute `size`
> my_func (f: Foo) = f\size
> my_func foo
10
> my_func foo + my_func (Bar with size=20)
30
> object Bloop <: Foo
## the above is equivalent to
> object Bloop <: Foo = Bloop f <: f
> :t Bloop (Bar with size=0)
Bloop (Bar with size=0) : Bloop
> object Blorp <: Foo = Bloopit <: Bar | Blip <: Baz 2
> blorp = Bloopit
Exception
  In the expression `blorp = Bloopit`
  In the constructor `Bloopit`
  ObjectError: object `Foo` requires attribute `size` (`with size=...`)
> object Blorp <: Foo = 
    Bloopit <: Bar with size=0 
  | Blip <: Baz 2 with size=1
  with name: Str
