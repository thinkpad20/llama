_.=_ : Ref a -> (a -> a) -> a
r .= f = r := f $r

++_ : Incr a. Ref a -> a
++r = r .= incr

_++ : Incr a. Ref a -> a
r++ = $r before r .= incr

trait Bool a = _? : a -> Bool

impl Bool for Maybe _ = 
  Nothing? = False
  (_)? = True
impl Bool for Bool = _? = id
impl Bool for List _ = 
  []? = False
  (_ ~ _)? = True
