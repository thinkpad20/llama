```
import Channel\{Channel, spawn}
import Random\rand
import Time\sleep

ch = Channel $ ->
  for i in 10.range
    println '#{i}th message from channel #{@channel\id}!'
    sleep $ rand (0, 3)

spawned = map spawn $ replicate 10 ch
while any is_alive spawned
  println 'Main thread not done...'
  sleep 1
println 'All channels finished! Exiting...'
```
