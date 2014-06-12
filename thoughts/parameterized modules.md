```
module Foo (message: Str) (number: Int)
exports do_thing

do_thing: Str -> () =
  s -> 
    println 'Message was: #[message], you wrote: #[s]'
    if s == message then println 'you win!'

do_thing: Int -> () = 
  i -> 
    println 'Number was: #{number}, you wrote: #{i}'
    if i == number then println 'you win!'

end Foo
```

We can now import `Foo` multiple times, even within the same module.

```
import Foo 'hello' 34
import Foo 'yo' 13 as Foo1

Foo.do_thing 'sup' # Message was: hello, you wrote: sup
Foo.do_thing 10    # Number was: 34, you wrote: 10
Foo1.do_thing 13   # Number was: 13, you wrote: 13
                   # you win!
```
