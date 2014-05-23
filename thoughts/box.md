trait Box = unbox : Self a -> a
implement Box for Maybe with
  unbox (Just x) = x
  unbox Nothing = throw Exception 'Nothing to unbox'
implement Box for Ref with unbox (Ref x) = x
implement Functor for Ref with map f (Ref x) = Ref (f x)
trait Update = (:=) : Self a -> a -> Self a
implement Update for Maybe with _ := x = Just x
