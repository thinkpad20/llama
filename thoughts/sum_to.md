Ints and references in Llama -> JavaScript:

```
sum_to n = !result after
  result = ref 0
  for i = ref 0; !i < n; ++i
    result += !i

```

```
function sum_to(n) {
  var result = ref($int(0));
  for (var i = ref($int(0)); $unbool($lt($access(i))(n)); $plus$plus(i)) {
    $plus$eq(result)($access(i));
  }
  return $access(result);
}
```
