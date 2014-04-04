Distinguish between script and module

Scripts are supersets of modules, but their goal is to do something when run. 

Example script:

```
#!/usr/local/bin/llama
script KillProcess

import Sys\args, Sys\exit!
import Regex\match

try do target = args 1
catch _ 
  println 'No target process specified in args'
  exit! 1

procs = @exec_str "ps aux" ! splitlines
for items in procs.sub 1 ! map (split ' ')
  cmd = items -1
  pid = items 1
  if cmd.match target
    try 
      @exec 'kill -9 #[pid]'
      println 'Killed #{target} (pid #[pid])'
      exit! 0
    catch IOError msg
      println 'Attempted to kill #{target} with pid #[pid], \
               but got error #{msg}'
      exit! 1

println 'No running process matches target #{target}.'
exit! 1
```

Running this script clearly has side-effects.

```
$ echo 'while 1: 0' >> silly.py
$ python silly.py &
$ KillProcess.llm 'python silly.py'
Killed "python silly.py" (pid 49539)
```

We then can say that a `module` is distinct from a `script` in that it's pure. This doesn't mean all of its functions are pure, but that running it has side-effects. Then if we tried to do this:

```
module Foo
println 'Hi, this is Foo!'
```

We would get a compile-time error.

How about scripts running other scripts?

```
# Foo.llm
script Foo
println 'Hey there, this is foo!'
```

```
# Bar.llm
script Bar
runimport Foo
println 'Hi, this is bar!'
```

Running it:

```
$ llama Bar.llm
Hey there, this is foo!
Hi, this is bar!
```

What if `Foo` also has things we want to import? (Note: it's probably best to do this as little as possible.)

```
script Foo
foo n = n + 1
println 'You just loaded foo.'
```

```
script Bar
runimport Foo\foo
println 'You loaded bar. #{foo 3}!'
```

Seems straightforward. Do we cache multiple imports? Seems like a good idea. Buuuut....

```
script Bar
runimport Foo\foo

println foo 3

close f after
  f = open ('Foo.llm', @mode='w')
  f.writeln 'script Foo\nfoo n = n + 2'
  println 'Modified Foo.llm!'

runimport Foo\foo

println foo 3
```

It looks like we're reloading Foo after modifying it? Do we support this?

```
$ llama Bar.llm
4
Modified Foo.llm!
5
```

Hmm, maybe not the best idea... maybe require imports to be at the top?

Anyway, it looks like we have these keywords: 

* `script`: declares an impure module
* `module`: declares a pure module
* `runimport`: imports an impure module and its contents, possible side effects
* `import`: imports a pure module, has no side effects

We might want to instead go with `script` -> `module`, `module` -> `pure module`, `runimport` -> `import!`, `import` -> `import`. That way the only new keywords we have are `import!` `import` and `module`.

What if we could be a little smarter, for example, a script which can be run as either a script or a module?

```
module Foo
foo n = n * 7
if @name == '__main__'
  println "Hey, I'm a script!"
```

That would be the python way, but it's a little arbitrary. What about something smarter?

```
module Foo
foo n = n * 7
@if_impure
  println "Hey, I'm a script!"
```

Then the above could be loaded as a module, *or* ran as a script. As long as every impure action is isolated in some kind of guard, we can check at compile time if an import is pure:

```
module Bar
import Foo # OK, side-effects don't occur
bar n = n.Foo\foo.show
```

```
module Baz
import! Foo # OK, has a side-effect
baz n = (Foo\foo n).show
```

This seems good.

By the way, there's an issue of top-level mutable variables in scripts. This means that if scripts get reloaded we could have issues. For example:

* Module `a` has a mutable variable `foo = 0`.
* Module `b` imports `a`.
* Then later, `a\foo` changes to `1`.
* At some point later, module `c` imports `a\foo`. Does it get the original value, `0`, or the new value, `1`?

Other things to consider:

* Versioned imports
* Imports that hide things, such as variables or instances
* Open imports
* Lazy imports (don't perform the inport until/unless used)
  - What about type checking?
    + 'Trusted imports', where we say some of what a module will have, without actually having that implementation work. For example:
      ```
      trust import Foo
        foo : Num -> Str
        bar : (Str, Str) -> [Num]
        impl SomeClass SomeType

      blorp x = Foo\bar (x, x) ! reverse
      ```
    + Then `Foo` wouldn't be actually imported until/unless `blorp` actually got called. At that point, `blorp` would be type checked and a runtime exception would be thrown if any of the things didn't exist, or weren't the types claimed.
  - What about imports that have side-effects? I guess that's OK, if not advisable?
* Imports as expressions, scoped imports
* What happens when imports conflict?
