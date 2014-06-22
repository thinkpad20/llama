typeof :: Expr -> TypeChecker (Subs, TypeSet)
typeof = \case
  Var name -> lookup name >>= \case
    Nothing -> error
    Just ts -> return (zero, ts)
  Define name type_ expr -> do
    update name type_
    (subs, ts) <- typeof expr
    -- verifies that type_ unifies with a member of ts
    subs' <- unifyTypeSet ts type_
    ret (subs • subs') (single type_)
  Apply e1 e2 -> do
    (s1, ts1) <- typeof e1
    (s2, ts2) <- typeof (s1 •> e2)
    result <- newvar
    let func = ts2 ==> result

