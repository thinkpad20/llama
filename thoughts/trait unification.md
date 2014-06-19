
```
Add:
  {}. [Int, Int, Int]
  {}. [List a, List a, List a]
  {Add a a a}. [Point a, Point a, Point a]
```

Then `[Int, Int, Int]` should satisfy, and `[Point τ, Point τ, Point τ]` should as well, provided that `[τ, τ, τ] ∈ Add`. Also `[List a, List a, List a]` for *any* `a`.

Test cases:


These should pass:
```
  [Int, Int, Int]
  [List Int, List Int, List Int]
  [Int, α, Int] (α is a type var)
  [Float, α, β]
  [Point Int, Point Int, Point Int]
```
These should fail:
```
  [List Int, List Str, List Int]
  [Int, Maybe α, Int]
```

Idea: let `unify` also handle context management. Then we can check instances while we `unify`.  This also means that we can use `unify` while we check instances.

```
unify :: Context -> Type -> Type -> TypeChecker (Substitution, Context)
unify ctx t1 t2 = do
  subs <- unify' t1 t2
  ctx' <- checkContext $ substitute subs ctx
  return (subs, ctx')
  where
    unify' t1 t2 = case (t1, t2) of
      (TVar a, _) -> only (a, t2)
      (_, TVar b) -> only (b, t1)
      (TConst n, TConst m) | n == m -> return zero
      (TApply t1 t2, TApply u1 u2) -> do
        subs1 <- unify' t1 u1
        subs2 <- unify' (substitute subs1 t2) (substitute subs1 u2)
        return (subs1 • subs2)

typeof (Apply a b) = do
  (s1, Type c1 t1) <- typeof a
  (s2, Type c2 t2) <- typeof (substitute s1 b)
  res <- newvar
  (s3, c3) <- unify (c1 <> c2) t1 (t2 ==> res)
  return (s1 • s2 • s3, substitute (s1 • s2 • s3) res)

unifyContext :: Context -> TypeChecker Substitution
unifyContext c@(Context ctx) = mconcat <$> mapM check ctx where
  check (HasTrait name ts) =
    if any isVar ts then return ()
    -- Otherwise, make sure it's in the TraitMap
    else search =<< getInstances
    where
      search [] = throwError1 $ "No instance of " <> whatsMissing
      search (tmap:rest) = case lookup name tmap of
        Just list -> searchList rest set
        Nothing -> search rest
      searchList rest = \case
        [] -> search rest
        (Entry context types:es) -> do
          tryEntry context zero (zip ts types)
            `catchError` \_ -> searchList es
      tryEntry context subs = \case
        -- If we're at the end of the list then we succeeded
        [] -> return subs
        ((inputT, storedT):ts) -> do
          -- Unify the input type with the stored one
          subs' <- unify inputT storedT
          -- Update the context and check it
          let context' = substitute subs' context
          subs'' <- checkContext context'
          -- Apply the substitutions to the rest of the types
          tryEntry context' $ substitute (subs' • subs'') ts
      whatsMissing = case ts of
        [] -> name
        [t] -> name <> " for type " <> render t
        ts -> name <> " for types " <> commaSep ts
      commaSep [t] = "and " <> render t
      commaSep (t:ts) = render t <> ", " <> commaSep ts
      isVar (TVar _) = True
      isVar _ = False
```

Point 1 + Point 2 ->
  subs = unify {Add a b c} (a -> b -> c) (Point Int -> d)
       = {a => Point Int, d => (b -> c)}
  context = substitute subs {Add a b c}
           = {Add (Point Int) b c}
  checkContext context


checkContext :: Context -> TypeChecker Substitution
checkContext {Add (Point Int) b c}
 -> check (Add (Point Int) b c)
   -> instances = get_instances Add
                = [({}, [Int, Int, Int]),
                   ({Add a a a}, [Point a, Point a, Point a])]
      -> search instances [Point Int, b, c]
        -> try to unify [Int, Int, Int] [Point Int, b, c] --> FAIL
        -> try to unify [Point a, Point a, Point a] [Point Int, b, c]
           -> unify (Point a) (Point Int)
             -> {a => Int}
           -> checkContext (substitute {a => Int}) {Add a a a}
             -> check {Add Int Int Int}
             -> search instances [Int, Int, Int]
               -> try to unify [Int, Int, Int] [Int, Int Int]
               -> {}
           ->
           -> unify (substitute {a=>Int} (Point a)) (substitute {a=>Int} b)
             -> unify (Point Int) b
             -> {b => Point Int}
           -> unify (substitute {a=>Int, b=>Int} (Point a))
                    (substitute {a=>Int, b=>Int} c)
             -> unify (Point Int) c
             -> {c => Point Int}
           -> {a => Int, b => Int, c => Int}
        -> success!
      -> {a => Int, b => Point Int, c => Point Int}

class Unify t where
  unify :: t -> t -> Substitution
instance Unify BaseType where
  unify = unifyBaseType
instance Unify a => Unify [a] where
  unify = fmap (compose . unify)
instance Unify Context where
  unify (Context ctx) = checkContext

























Satisfying conditions:
1) Any of the types in the tuple are type variables
2) Is a member of the set
3)
