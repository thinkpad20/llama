Church encoding! :)

```
object Expr = 
  Var Name
  Apply Expr Expr
  Lambda Name Expr

lx = Lambda "x"
zero = lx (Var "x")
one = lx zero
two = lx one
three = lx two = lx (lx one) = lx (lx (lx zero))

one.append two = 
(lx zero).append (lx one) = 
(lx zero).append (lx (lx zero)) = 

impl Monoid Expr = 
  empty = zero
  'zero.append x = x
  x.append 'zero = x
  ('lx x).append ('lx y) = (x.append y).lx.lx
```
