```
trait Callable obj idx -> res =
  @call obj -> idx -> res

impl Callable [!a] Int a = 
  @call list = 0 => head list
             | i => @call (tail list) (i - 1)

foo: [!Int] = [! 1, 2, 3, 4]

bar: Int = foo 1

```
