
```
/* Does some blibber blobishness */
import os
import fs\open, fs\contents

print_env : String -> ()
print_env key = 
  /*Given an environment variable, prints it or says it's not there*/
  try
    println os\environ "FOO_BAR"
  catch KeyError _
    println "No environment variable #{key}"

then_just : Bool -> lazy a -> Maybe a
then_just pred (lazy action) = if pred then Just action else Nothing

get_file : FilePath -> Maybe String
get_file path = path.exists.then_just path.open.contents
```
