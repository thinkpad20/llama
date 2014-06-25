Let's start by assuming unary instances only, for simplicity. Also, let's assume we don't have flexible instances. Then what can we do?

```
trait Add a = _+_: a -> a -> a
trait Eq a = _==_: a -> a -> Bool
impl Add for List a
  _+_ = append
impl Add for Nat with
  Z + n = n
  S n + m = S (n + m)
# Not an efficient implementation here!
impl Eq for Int with
  0 == 0 = True
  n == m if n > 0 && m > 0 = n-1 == m-1
  n == m if n < 0 && m < 0 = n+1 == m+1
  _ == _ = False
impl Eq for Nat with
  Z == Z = True
  S n == S m = n == m
  _ == _ = False
impl Eq for {Eq a}. List a
  l1 == l2 = l1.length == l2.length && alltrue (zipwith _==_ l1 l2)
```

So we have some traits and some instances. Now how do we check if an instance is valid?

```
[1,2,3] == [1,2,3]
```

This should typecheck. How? Well, `[1,2,3]` is of type `List Int`, and there is an instance of `Eq` for `Int`. When we are checking the assertion `Eq (List Int)`, we see a matching instance `{Eq a}. List a`...

```
match :: Context          -- ^ The instance's context
      -> BaseType         -- ^ The stored instance's type
      -> BaseType         -- ^ The type in the assertion
      -> TypeChecker Bool -- ^ If it's a match
match _ _ (TVar _) = return True
match _ (TConst c) (TConst c') | c == c' = return True
match ctx (TApply a b) (TApply a' b') = do
  res1 <- match ctx a a'
  res2 <- match ctx b b'
  return $ res1 && res2
match ctx (TVar a) (TConst c) = ?
```

In the fourth case, we would want to return true if the constant satisfied the constraints of the stored type. For example, in the case we described above, we'd be looking at `match ({Eq a}) (List a) (List Int)`, which would go to `match {Eq a} a Int`. At this point, we're back to our original problem, namely, checking if some type (`Int`) is an instance of some trait (`Eq`). So how do we do that? We look through all of the instances we have stored for `Eq`, and return if one of them matches with `Int`. The first thing we need to do is to generate a new context: by unifying `a` with `Int`, we can create a new context: `{a => Int} •> {Eq a} = {Eq Int}`. Then we check that context.

```
match ctx (TVar a) (TConst c) = do
  subs <- unify (TVar a) (TConst c)
  validate (subs •> ctx)
  return True
```

At this point it seems clear that returning a Bool is not very helpful: after all, the thing either succeeds or fails. If it succeeds, it should produce something useful: for example, it could produce a *new* context:

```
# "Partial matches" get propagated out
validate {Eq (List a)} => {Eq a}

# Constant assertions can be removed
validate {Eq (List Int)} => {}

# False assertions get caught
validate {Eq (Int -> String)} => ERROR
```

That suggests the following signature:

```
validate :: Context -> TypeChecker Context
```

So let's see how that might look:

```
validate (Context ctx) = fmap mconcat $ mapM assertionToContext ctx

assertionToContext :: Assertion -> TypeChecker Context
assertionToContext (HasTrait name types) = do
  -- for each instance stored for `name`
    -- for each type in the list of types
      -- try to generate a new context with `match`
      -- if this fails, move on
  -- if no matches, assertion failed; throw an error
  instanceLists <- getInstances name
  goILs instanceLists
  where
    -- Iterating over a list of instance lists
    goILs [] = throwError "No matching instance"
    goILs (il:ils) = goIL il `catchError` \_ -> goILs ils
    -- Iterating over a list of instances
    goIL [] = throwError "Nothing matched in this instance list"
    goIL (Instance ctx types':is) = goT ctx types` `catchError` \_ -> goIL is
    -- Iterating over a list of types
    goT types' = fmap mconcat . uncurry (match ctx) $ zip types types'

match :: Context             -- ^ The instance's context
      -> BaseType            -- ^ The stored instance's type
      -> BaseType            -- ^ The type in the assertion
      -> TypeChecker Context -- ^ The new context
match ctx instType asrtType = do
  subs <- unify' instType asrtType
  validate (subs •> ctx)
  where
    unify' iT aT = case (iT, aT) of
      (_, TVar a) -> oneSub a iT

```


  That stored type might be an unrestricted type variable, in which case we're good to go, or it might be restricted in some way, in which case the constant needs to satisfy the restrictions, or it might be another constant, in which case it needs to be exactly equal. Let's case these out:

```
match (TConst c) (Type ctx (TVar v)) | not (isRestricted ctx v) = return True
match (TConst c) (Type _ (TConst c')) | c == c' = return True
                                      | c =/= c' = return False
```
