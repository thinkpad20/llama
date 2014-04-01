Completely dynamic typing is sometimes nice

```
magic MyMagic = 
  MyMagic (**kwargs) {
    for key in kwargs.keys
      self.set key, kwargs key
  }

foo = MyMagic (@foo=1, @bar=2)

> :t foo
MyMagic
> :t foo\bar
Num
> :t foo\blibber
```

Such as for shell...

```
magic Shell = Shell
  with @getattr self name (*args: [Str], @_show=False) = res after
    mut proc = Sys\proc $ (name.append args).joinBy " "
    for line in proc.lines
      res.append line
      when _show do println line

sh = Shell
sh\ls "/Users/anelson" ! sh\grep "*.txt"
```

But realistically, we can probably just get away with

```
# execute shell command, show output
shell : Str -> Str

# execute shell command, pipe first arg into stdin, return string
pipe_ : File -> Str -> Str

# execute shell command, hide output but return stdout file handle
shell_ : Str -> File

# execute shell command, pipe first arg into stdin, return handle
pipe_ : File -> Str -> File

((shell_ "ls /Users/anelson").pipe_ "grep *.txt").pipe "wc -l"
# or
shell_ "ls /Users/anelson" !.pipe_ "grep *.txt" !.pipe "wc -l"
```
