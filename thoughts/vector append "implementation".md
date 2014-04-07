let current_id = ref 0

let next_id () =
  let id = !current_id in
  current_id := id + 1 ;
  id


current_id = ref 0

next_id () = current_id := read current_id + 1
next_id () = current_id += 1

This isn't a real implementation... and it's only one for an array, not a persistent vector (of course)

```
append! (v: ref [a]) (x: a) = read v after
  if v\length == v\real_length
    v\real_length *= 2
    mut new_array = SYSTEM\alloc real_length
    for i in v\length.range do new_array.insert_at! (i, v i)
    v\_array := new_array
  v\_array[: v\length] := x
  v\length += 1
```
mut foo = [1,2,3]
println foo.append! 4
