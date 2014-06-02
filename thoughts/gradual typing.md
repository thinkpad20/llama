Conceptualizing a gradual typing algorithm for Llama:

First is collecting constraints. Something along these lines perhaps?

```haskell
data Constraint
  = Type `Accepts` Type
  | Type `ResultsFrom` Type Type
  | Type `IsExactly` Type
  | Type `UnifiesWith` Type

getConstraints :: Expr -> m Type
getConstraints e = case unExpr e of
  Var name -> lookupType name >>= \case
    Just _type -> return _type
    Nothing -> throwError $ "Undefined variable " <> name
  Define name e -> do
    when (isInScope name) $ throwError $ "Redefinition of " <> name
    newvar <- newTypeVariable
    register name newvar
    eT <- getConstraints e
    addConstraint $ newvar `IsExactly` eT
  Apply a b -> do
    aT <- getConstraints a
    bT <- getConstraints b
    resultT <- newTypeVariable
    addConstraint $ aT `Accepts` bT
    addConstraint $ resultT `ResultsFrom` aT bT
    return resultT
  Lambda name Nothing e -> withContext name Any $ getConstraints e
  Lambda name (Just _type) e -> withContext name _type $ getConstraints e
  Typed e _type -> do
    eT <- getConstraints e
    addConstraint $ eT `UnifiesWith` _type
  a `Then` b -> getConstraints a >> getConstraints b
```

Next, we need to solve these constraints, and detect if there are any contradictions. If not, we're good to go.
