baz : Int -> String
baz : Int -> [Int]
baz : Int -> Int
(>) : Int -> String -> Bool

with Show a
  println (x: a; output: File=Sys\STDOUT): () = 
    map output.put_char (x.show + '\n')
  # The key 'output' is part of the tuple's type
  print: (a; output: File) -> () = 
    arg -> 
      output = arg\output || Sys\STDOUT
      x = arg\0
      map output.put_char (arg\0.show)

foo: Int -> Int -> Int =
  bar = baz 6 # could be (Int, String or [Int])
  i -> 
    if i > bar # in condition, must be Bool, so bar must be String.
      baz 10   # Last statement in expr, must be Int.
    else
      println 'oh poop'
      foo bar-1 i # (-) is only defined for Int->Int->Int, so bar must be Int.
