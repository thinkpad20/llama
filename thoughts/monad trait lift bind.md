```
trait Lift m = lift : a -> m a
trait Bind m = bind : m a -> (a -> m b) -> m b
trait Monad m = {Lift m, Bind m}

foo : Lift m => Int -> m Int
foo n = lift (3 + n)
 
~foo : (a -> m a) -> Int -> m Int
~foo lift n = lift (3 + n)

impl Lift Maybe = lift = Just
impl Bind Maybe = .f = Nothing => Nothing
                     | Just x => f x

bar : (Lift m, Bind m) => Int -> m (Int -> Str) -> m Str
bar : Monad m => Int -> m (Int -> Str) -> m Str
bar n f = ((lift n).bind n => n + 1).bind (apply f)

~bar : (a -> m a) -> (m a -> (a -> m b) -> m b) -> Int -> m (Int -> Str) -> m Str
~bar lift bind n f = ((lift n).bind n => n + 1).bind (apply f)
```
