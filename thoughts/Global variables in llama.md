Some ideas for global variables in llama

```
@status : Maybe Int    // status of last shell command run
@path : Str            // current directory
@path_of : a -> Str    // path of module where object is
@exec : Str -> Int     // run the shell command given, return status
@exec_str : Str -> Str // run shell command, return stdout
@name : Str            // the name of the module
@module : Module       // the current module
```

Note that none of these can be mutated by the user, even though several are mutable.

A crappy script that uses them

```
#!/usr/bin/llama
list_files dir = "ls #[dir]".@exec_str.split_lines

rm_foos () =
  '''Removes all of the foos'''
  for file_path in list_files @path
    if file_path.starts_with "foo" 
      println "Removing #{file_path}"
      @exec "rm #[file_path]"

if @name == '@main' do rm_foos()
if @status != Just 0 println "Failed! :("
```
