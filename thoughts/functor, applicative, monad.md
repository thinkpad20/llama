```llama
map: (f: a->b) (ma: Maybe a): Maybe b = case ma of
  Nothing -> Nothing
  Just x -> Just f(x)

lift: a -> Maybe a = Just

(<*>): (f: Maybe (a -> b)) (ma: Maybe a): Maybe b = case f, ma of
  Nothing, _ | _, Nothing -> Nothing
  Just f, Just x -> Just f(x)

(>>=): Maybe a -> (a -> Maybe b) -> Maybe b = 
  Nothing -> _ -> Nothing
  Just a -> f -> f a

trait Functor f = map: (a -> b) -> f a -> f b

trait Lift f = lift: a -> f a

trait Applicative f = 
  Functor f
  Lift f
  (<*>): f(a->b) -> f a -> f b
  
trait Monad m = 
  Applicative m
  (>>=): m a -> (a -> m b) -> m b
```
