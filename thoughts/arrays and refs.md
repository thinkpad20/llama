foo = array [1, 2, 3]
bar = foo[2]
bar := 10
baz = foo 2
assert(baz == 2; msg = 'baz should equal 2')

foo = do println 'hey'; 2

with_file (loc: FilePath; mode='r') (f: File -> a): a =
  file = open (loc, mode)
  f file before file.close

save (file: File) (; location: Str) = 
  save_to = case location of Nothing -> file\source; Just loc -> loc
  with_file (save_to, 'w') (.write file\contents)

file.save()
file.save('foo.txt')
file.save(; location='foo.txt')

(.=) (r: Ref a) (f: a -> b): b = a := !a.f
trait Incr with incr : Self -> Self
implement Incr for Int with incr = (i -> i + 1)
incr! (i: Ref a of Incr): a = i .= incr
