```
module Foo
object Foo = Foo Str
```

```
module Bar
import Foo
impl Show Foo = 
  show (Foo s) = 'A foo containing the string #{s}'
bar = 3
```

```
module Baz
import Foo
impl Show Foo = 
  show (Foo s) = 'Dis be a foo #{s}, foo'
bar = "hey"
```

```
module Qux
import Bar\(impl Show Foo)
import Baz\bar

foo = Foo 'hey #[bar]'
println foo
```

```
$ llama Qux.llm
A foo containing the string "hey hey"
```
