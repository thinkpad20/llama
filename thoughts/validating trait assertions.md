rule: if a type variable was created in this context, we can remove it from the substitutions when we return.

```
typeof (x -> x + x)
  store x (newvar = t0)
  typeof (x + x)
    typeof (_+_ x)
      typeof _+_ 
        -> {Add t1 t2 t3}. t1 -> t2 -> t3
      typeof x 
        -> t0
      res = newvar = t4
      subs = unify (t1 -> t2 -> t3) (t0 -> t4)
           = {t1 => t0, t4 => t2 -> t3}
      -> ({}, {Add t0 t2 t3}. t2 -> t3)
    typeof x
      -> t0
    res = newvar = t5
    subs = unify (t2 -> t3) (t0 -> t5)
         = {t2 => t0, t3 => t5}
    -> ({}, {Add t0 t0 t5}. t5)
  ({}. {Add t0 t0 t5}. t0 -> t5)
```

So what if we have a type constant?

```
typeof (x -> x + 1)
  store x (newvar = t0)
  typeof (x + 1)
    typeof (_+_ x)
      typeof _+_ 
        -> {Add t1 t2 t3}. t1 -> t2 -> t3
      typeof x 
        -> t0
      res = newvar = t4
      subs = unify (t1 -> t2 -> t3) (t0 -> t4)
           = {t1 => t0, t4 => t2 -> t3}
      -> ({}, {Add t0 t2 t3}. t2 -> t3)
    typeof 1
      -> Int
    res = newvar = t5
    subs = unify (t2 -> t3) (Int -> t5)
         = {t2 => Int, t3 => t5}
    -> ({}, {Add t0 Int t5}. t5)
  ({}. {Add t0 Int t5}. t0 -> t5)
```

What if we have a type constant without instances?

```
typeof (1 + 'hey')
  typeof (_+_ 1)
    typeof _+_
      -> {Add t0 t1 t2}. t0 -> t1 -> t2
    typeof 1
      -> Int
    res = newvar = t3
    subs = unify (t0 -> t1 -> t2) (Int -> t3)
         = {t0 => Int, t3 => t1 -> t2}
    -> ({}, {Add Int t1 t2}. t1 -> t2)
  typeof 'hey'
    -> String
  res = newvar = t4
  subs = unify (t1 -> t2) (String -> t4)
       = {t1 => String, t2 => t4}
  -> ({}, {Add Int String t4}. t4)
```

This is well-typed in itself, but assuming there's no instance matching `Int String t`, we'd get a compile-time error if this was a top-level expression. Resolving ambiguities goes hand-in-hand with looking up instances: we should delay that until we actually need to determine *which value to use*. We simply gather a set of assertions, and wait to test them until we have to.

We can get some errors early, though:

```
trait Mult a = _*_: a -> a -> a
typeof (1 * 'hey')
  typeof (_*_ 1)
    typeof _*_
      -> {Mult t0}. t0 -> t0 -> t0
    typeof 1
      -> Int
    res = newvar = t3
    subs = unify (t0 -> t0 -> t0) (Int -> t3)
         = {t0 => Int, t3 => Int -> Int}
    -> ({}, {Mult Int}. Int -> Int)
  typeof 'hey'
    -> String
  res = newvar = t4
  subs = unify (Int -> Int) (String -> t4)
       = ERROR: can't unify Int and String

trait Negate a = -_: a -> a
typeof (-'hello')
  typeof -_
    -> {Negate t0}. t0 -> t0
  typeof 'hello'
    -> String
  res = newvar = t1
  subs = unify (t0 -> t0) (String -> t1)
    -> {t0 => String, t1 => String}
  -> {Negate String}. String
```

In the first example, because of the signature of `_*_`, we know that the remaining arguments must match the type of the first one, so we error when we see a string. In the second example, we end up with the assertion that `String` is in the type set of `Negate`, which is false.

I think it's good to remember that each trait constraint is an assertion. The assertion is either a contradiction, which results in an error, ambiguous, if more than one instance matches and there is no default, or valid, if there is exactly one matching instance. The question is, at what point to we validate these assertions? We can definitely validate them as soon as they're entirely constant (e.g. `Negate String`), but this isn't comprehensive.

We basically need to validate them when we need to use them. For example, the assertion `Negate String` might be false at the point at which it's declared, but it might be valid by the time it's used:

```
foo x = -(x.show)               # foo : {Negate String, Show a}. a -> String
-_ : String -> String = reverse
println foo(12345)            # '54321'
```

When we typed `foo`, there was no satisfying instance, but when we *used* it, there was. The type of the expression `println foo('hello')` is `{Negate String, Show Int}. ()`. Since this is a top-level expression, we need to type it.

Of course, because we have side-effects, we can't express everything just in the type system. We might need to be smarter.

We can imagine some functions `validate` and `resolve`:

```haskell

validate :: Context -> TypeChecker Substitution
validate (Context ctx) = fmap compose $ mapM resolve ctx

resolve :: Assertion -> TypeChecker Substitution
resolve (HasTrait name types) =
  instances <- matchingInstances name types
  case instances of
    [] -> throwError "No matching instances"
    (_:_:_) -> defaultInstance name types >>= \case
      Nothing -> throwError "Ambiguous instances"
      Instance context types' -> (•> types') <$> validate context
    [Instance context types'] -> (•> types') <$> validate context
```

The types are a little fuzzy, but the idea is:
  * For each assertion in the context:
    - Find all instances matching the assertion.
      + For example, if the assertion is `Add Int Int a`, it might find `Instance {} [Int, Int, Int], Instance {} [Int, Int, Float], Instance {IntLit a}. [Int, Int, a]`.
    - If there are no matching instances, it's an error.
    - If there are multiple, there must be a default instance which can be determined unambiguously.
    - If there is exactly one matching instance, check its context. For example, if the assertion is `Eq [Float]`, then the matching instance would be `{Eq a}. [a]`. We would need to unify this instance with our given type, to get `a => Float`, and then validate the context `{Eq Float}`.

The thing that's a little iffy about this is the matching instances part. Need to thinking about it.

To find a matching instance, we need to have that all types in the instance can be unified with all types in the assertion.

For example, `Assert "Add" a b c` matches whatever instances we have. `Assert "Add" Int b c` matches `Instance {} Int Int Int`, but not `Instance {} Float Int Float`. `Assert "Eq" [Int]` matches `Instance {Eq a} [a]`.

```
verify :: Context -> TypeChecker Substitution
verify (Context ctx) = fmap compose $ forM ctx $ \assertion ->
  instances <- findInstances assertion
  case instances of 
    [] -> throwErrorC ["No matching instances"]
    [Instance types]

matchInstance :: Instance -> Assertion -> TypeChecker (Maybe Substitution)
matchInstance (Instance ctx ts) (Assert _ tv) = go zero ctx (zip ts tv) where
  go subs _ [] = return $ Just subs
  go subs ctx ((instanceT, assertT):rest) = do
    subs' <- unify instanceT assertT
    subs'' <- verify (subs' •> ctx)
    let newSubs = subs • subs' • subs''
    go newSubs (newSubs •> ctx) rest
    `catchError` \_ -> return Nothing
```
