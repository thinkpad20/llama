times (a of Monoid) (n: Num) = result after
  mut result = zero: a
  for i in n.range do result .= append a

```

var times = function(a) {
  return function(n) {
    var result = zero.cast(a.type);
    var _ref = range(n);
    var _iter = iter(range(n));
    for (; valid(_iter); _iter = next(_iter)) {
      var i = get(_iter);
      result = append(result)(a);
    }
    return result;
  }
}
