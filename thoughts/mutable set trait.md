trait MutableCollection = 
  add    : Self a -> a -> ()
  remove : Self a -> a -> ()
trait Listable = 
  from_list : List a -> Self a
  to_list   : Self a -> List a
trait Identity = id : Self
trait Join = join : Self -> Self -> Self
trait Monoid = Identity; Join
trait Container =
  empty    : Self a
  contains : Self a -> a -> Bool
trait Functor = map : (a -> b) -> Self a -> Self b
trait MutableSet = Container; MutableCollection
  
