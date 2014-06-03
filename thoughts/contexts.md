Use contexts to temporarily name variables, associate variables with types, or name traits.

We can simulate OOP with this:

```
type Position (x: Int) (y: Int)
context self: Position
  self + (Position x y) = Position (self.x + x) (self.y + y)
  self.show = '(#{self.x}, #{self.y})'

type Shape = 
  Rectangle (height: Float) (width: Float)
  Circle (radius: Float)
  with pos: Position

context self: Shape
  self.show = 
    case self of
      Rectangle height width -> 'Rectangle, height #{height}, width #{width}'
      Circle radius -> 'Circle with radius #{radius}'
    + ', located at #{self.pos}'
  self.move (delta: Position) = self with pos=(self.pos + delta)
```

Or we can name variables:

```
context {foo x = do_something_complicated_to x,
         bloop y = another_complicated_expression y}
  case bloop 1 of
    (a, 0) -> throw Error('aaaaaah')
    (a, b) -> foo a + foo b
```

Or create a trait:

```
context Size = trait a {size: a -> Int}
  show_size a: _ of {Size, Show} = '#{a} with size #{a.size}'
```

Any bindings in a context are valid in the context, but not out of it. But whatever expressions or definitions occur in a context are exported to the outer scope.

