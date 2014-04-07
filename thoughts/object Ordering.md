object Ordering = LT; GT; EQ
object Error => NotImplementedError
object Str => MyString = s => MyString s

trait Eq a => Comp a = {
  compare : a -> a -> Ordering;
  (<) : a -> a -> Bool;
  a < b = compare a b == Lt;
  (>) : a -> a -> Bool;
  a > b = compare a b == Gt;
}

trait Eq a = (==) : a -> a -> Bool;

(<=) : (Eq a, Comp a) -> a -> a -> 
x <= y = x < y || x == y;
