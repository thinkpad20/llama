sum_to (n: Int): Int = sum! after
  foo = ref 0
  sum = ref 0
  while foo! < n
    sum += foo!

sum_to (n: Int): Ref Int = sum after
  foo = ref 0
  sum = ref 0
  while foo! < n
    sum += foo!  

->

foo = 0
sum_to = Function()
add_instance(sum_to, function(n)
  foo = 0
  sum = 0
  while foo < n
    sum += foo
  end
  sum
end)

function sum_to(n::Int64)
  i = 0
  sum = 0
  while i < n
    sum += i
    i += 1
  end
  sum
end
