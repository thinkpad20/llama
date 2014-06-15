# Unification of trait constraints

We need to be able to propagate, and check the correctness of, trait constraints attached to types. For example:

```
t1 = {IntLiteral a}. a
t2 = {}. b
  unify t1 t2 = Right {a => {IntLiteral b}. b}
```

Two type variables can unify with a type that combines their contexts. Otherwise, a type variable can only unify with another type IF that other type doesn't conflict with its context.

But we don't put `Type`s in substitutions, only `BaseType`s, which have no context. How can we deal with that?

Also, traits can disappear. For example:

```
t3 = {Add a a b}. a -> b
t4 = {}. Int -> Int
  unify t3 t4 = Right {a => {}. Int, b => {}. Int}
```

So it seems like we should be checking that contexts are satisfied when we do substitutions, not when we make substitutions.

Let's look at a "real" example:

```
trait Negate a = negate: a -> a
negate 1
```

How would we type this? Let's look at the basic type checking code:

```
typeof env (Apply e1 e2) = do
  (subs1, Type ctx1 t1) <- typeof env e1
  (subs2, Type ctx2 t2) <- typeof (substitute subs env) e2
  var <- newvar
  subs3 <- unify t1 (t2 ==> var)
  return (subs1 <> subs2 <> subs3, substitute subs3 var)
```

* `1` instantiates to `{IntLiteral t1}. t1`
* `negate` instantiates to `{Negate t2}. t2 -> t2`
* create a new type variable `t3` for the result of the application
* unify `t2 -> t2` with `t1 -> t3`
  - Unifying `t2, t1` gives us `{t2 => t1}`
  - unify `subtitute {t2=>t1} t2` which is `t1` to `subtitute {t2=>t1} t3` which is `t3`. So unify `t1` and `t3`, giving us `{t1 => t3}`.
  - so unify would return the substitutions `{t2 => t3, t1 => t3}`
* return the substitutions `{t2 => t3, t1 => t3}`, and the type `t3`.

Here we're losing the trait information!! How can we fix that? Should the substitutions contain the trait information? Or when we perform the application? We could do something like this:

```
typeof env (Apply e1 e2) = do
  (subs1, Type ctx1 t1) <- typeof env e1
  (subs2, Type ctx2 t2) <- typeof (substitute subs env) e2
  var <- newvar
  subs3 <- unify t1 (t2 ==> var)
  let result = substitute subs3 $ Type (ctx1 <> ctx2) var
  return (subs1 <> subs2 <> subs3, result)
```

Let's see how this would work in the above case.
* `1` instantiates to `{IntLiteral t1}. t1`
* `negate` instantiates to `{Negate t2}. t2 -> t2`
* create a new type variable `t3` for the result of the application
* unify `t2 -> t2` with `t1 -> t3`
  - Unifying `t2, t1` gives us `{t2 => t1}`
  - unify `subtitute {t2=>t1} t2` which is `t1` with `subtitute {t2=>t1} t3` which is `t3`. So we are unifying `t1` and `t3`, giving us `{t1 => t3}`.
  - so unify would return the substitutions `{t2 => t3, t1 => t3}`
* to make `result`, substitute those subs onto `{IntLiteral t1, Negate t2}. t3`.
  - `IntLiteral t1` becomes `IntLiteral t3`
  - `Negate t2` becomes `Negate t3`
* return the substitutions `{t2 => t3, t1 => t3}`, and the type `{IntLiteral t3, Negate t3}. t3`.

This works! In this example anyway. Proving it in the general case is tougher.

But this only would work if there were no incorrect trait applications. What do I mean? Well, what if instead of `a of IntLiteral`, the numeral `1` was of type `Int`? We don't have an instance (yet) of `Int` for `Negate`, so if we tried to apply `negate` to `1`, we should get an error. How might we detect this and call it an error?

What if we detected it after substitution? Let's see how the steps would go:
* `1` returns `Int`
* `negate` instantiates to `{Negate t1}. t1 -> t1`
* create a new type variable `t2` for the result of the application
* unify `t1 -> t1` with `Int -> t2`
  - Unifying `t1, Int` gives us `{t1 => Int}`
  - unify `subtitute {t1=>Int} t1` which is `Int` to `subtitute {t1=>Int} t2` which is `t2`. So unify `Int` and `t2`, giving us `{t2 => Int}`.
  - so unify would return the substitutions `{t1 => Int, t2 => Int}`.
* to make `result`, substitute those subs onto `{Negate t1}. t2`.
  - `Negate t1` becomes `Negate Int`
  - so our result type is `{Negate Int}. Int`
* At this point, we can check the context for contradictions. Each item in the context is an assertion, after all, which must be satisfied by its environment (e.g. `Γ ⊨ Negate Int`). Put simply, we check if we actually *have* those instances.
* To do that, we look at the instances we have of `Negate`, which means we need to have a dictionary of instances.* return the substitutions `{t2 => t3, t1 => t3}`, and the type `{IntLiteral t3, Negate t3}. t3`.

So our code now looks like:

```haskell
typeof env (Apply e1 e2) = do
  (subs1, Type ctx1 t1) <- typeof env e1
  (subs2, Type ctx2 t2) <- typeof (substitute subs env) e2
  var <- newvar
  subs3 <- unify t1 (t2 ==> var)
  ctx' <- checkContext env $ substitute subs3 (ctx1 <> ctx2)
  return (subs1 <> subs2 <> subs3, Type ctx' $ substitute subs3 var)
```

So now we need to figure out what `checkContext` should do. It should probably have this kind of type signature:

```
checkContext :: TypeEnv -> Context -> TypeChecker Context
```

The only information it conveys is success or failure, so we don't need to have it return anything, but for simplicity, we might as well just have it return the context it got. So the first line would look something like

```
checkContext env ctx = return ctx <* do -- something!
```

What do we have to do? A constraint is an *n*-ary relation on types. That is, for a trait `Foo t1 t2 ... tN`, there is a set of tuples of types (lists actually), and to be satisfied, the tuple `t1, t2, ..., tN` must be in the set.

However, if any of the types in that list are type *variables*, then we can let it slide: those can be "filled in" at a later date (e.g. we optimistically assume that when the function actually gets called, all of these types will exist). So we only need to throw an error if:
* The type tuple contains no type variables, and
* The type tuple does not appear in the trait's type set.

Let's assume our environment is this:

```haskell
type TypeMap = Map Name Type
type TraitMap = Map Name (Set [BaseType])
newtype TypeEnv = TypeEnv [(TypeMap, TraitMap)]
```

We use a list because it gives us an easy stack, instead of having to add/remove variables from a map. This also lets us do "temporary" instances of traits.

So now we can write `checkContext`:

```haskell
checkContext (TypeEnv env) c@(Context ctx) = mapM_ verify ctx *> return c where
  verify :: Trait -> [TraitMap] -> TypeChecker ()
  verify (Trait name ts) traitmaps =
    -- If there are any free variables, we assume it's valid
    if not . null $ freevars t then return ()
    -- Otherwise, make sure it's in the TraitMap
    else search name ts traitmaps
    where
      search [] = throwError $ "No instance of " <> whatsMissing
      search (tmap:rest) = case M.lookup name tmap of
        Just set | S.member ts set -> return ()
        Nothing -> search rest
      whatsMissing = case ts of
        [] -> name
        [t] -> name <> " for type " <> render t
        ts -> name <> " for types " <> commaSep ts
      commaSep = intercalate ", " . map render
```
