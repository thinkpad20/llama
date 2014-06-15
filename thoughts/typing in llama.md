```
get_one: Set a -> a = set ->
  if set.size == 0 then throw "No compatible types"
  if set.size > 1 then throw "ambiguous type"
  set.tolist.head

with unify: Type -> Type -> Subs
     unify: Type -> Set Type -> Subs
typeof: Env -> Expr -> (Set Type, Subs) = env ->
  Var n if n in env -> env n, {}
  Var n -> throw '#{n} is undefined'
  Apply e1 e2 ->
    ts1, subs1 = typeof env e1
    ts2, subs2 = typeof (subs1 env) e2
    t1, t2 = get_one {t1, t2 for t1, t2 in ts1.cross ts2 if cbu t1 t2}
    subs t1, subs after subs = subs1 + subs2 + unify t1 t2
  Lambda name body ->
    v = env.newvar
    ts, subs = typeof (env + {name=>v}) body
    (v ==> {ts.get_one}), subs
  Define name expr ->
    v = env.newvar
    ts, subs = typeof (env + {name=>v}) expr
    subs' = unify v ts.subs
```
