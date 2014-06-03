```
x: Ref(a) + y: Ref(a) = !x + !y
x: Ref(a) + y: a = !x + y
x: a + y: Ref(a) = x + !y
x: Ref(a) += y: a = x := !x + y
x: Ref(a) += y: Ref(a) = x := !x + !y

x: Ref(a) == y: Ref(a) = x\address == y\address
x: Ref(a of Eq) == y: Ref(a) = x == y
with t = a of {(==): a -> a -> Bool}
  a: t =/= b: t = not (a == b)

foo x: Num : Num = x + 1

while(gameLoop) square =/= finalSquare
  if ++diceRoll == 7 then diceRoll := 1
  case square +! diceRoll of
    'finalSquare ->
      # diceRoll will move us to the final square, so the game is over
      break gameLoop
    newSquare if newSquare > finalSquare ->
      # diceRoll will move us beyond the final square, so roll again
      continue gameLoop
    _ ->
      # this is a valid move, so find out its effect
      square += diceRoll
      square += board square

println "Game over!"
```
