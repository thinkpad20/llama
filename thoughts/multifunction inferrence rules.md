## Inferrence rules for multiple dispatch

Definition of symbols:
* `v : t` means that value `v` is of type `t`. Similarly the function `typeof(v)` gives the type of the value `v`. Values have exactly one type. Expressions can be considered to have more than one type.
* `e::t` is an operation on an expression `e` with `e ~ t`, and a type `t`,which produces a *value* of type `t`.
* `values(e)` refers to the set of values referred to by `e`. This is constructed from the definition(s) of `e`, if `e` is a variable, or otherwise from how `e` came about.
* `types(e)` refers to the set of types constructed from all of the values referred to by `e`. In other words, `types(e) = {typeof(v) | v ∈ values(e)}`
* `e ~ t` means the expression `e` can be considered to be of type `t` (possibly along with other types). Another way to state it is that the type `t` appears in e's type set. Concretely, `e ~ t iff ∃v ∈ values(e). typeof(v) = t`.
* `cbu(t1, t2)` if and only if types `t1` and `t2` can be unified. I.e. `unify(t1, t2) ≠ ⊥`.
* `e ≅ t` is a weaker statement than `e ~ t`. It means that there is some value in `e` whose type can be unified with (not necessarily which is equal to) `t`. Concretely, `e ≅ t iff ∃v ∈ values(e). cbu(typeof(v), t)`. Note that `cbu(t, t)` for all `t`, so `e ~ t ==> e ≅ t` for all `e, t`. The reverse is not necessarily true.
* `update(Γ, e, v, t)` takes an environment, an expression `e`, a value `v`  and a type `t` where `v: t`, and produces a new environment where `v` and `t` have been added to the value and type set referred to by `e`.

### Inferrence rules

Any expression is consistent with any of the types in its type set (follows from the definition of `~`):

```
 Γ ⊢ t ∈ types(e)
------------------
     Γ ⊢ e ~ t
```

An expression can be coerced into a value with a specific type with the `::` operator:

```
     Γ ⊢ t ∈ types(e)
--------------------------
      Γ ⊢ (e::t) : t
```

Values applied to each other is a straightforward rule:

```
 Γ ⊢ v1: t1 -> t2 AND v2: t1
--------------------------------
        Γ ⊢ v1 v2 : t2
```

Lambda expressions:

```
  Γ ∪ {e1 ~ t1} ⊢ e2 ~ t2
---------------------------
 Γ ⊢ (e1 -> e2) ~ t1 -> t2
```

We can apply *f* to *e* if either
* *e* is consistent with a type which is an argument in *f*, or
* *e* can be unified with an argument type in *f*.

```
     Γ ⊢ t1 -> t2 ∈ types(f) AND e ≅ t1
---------------------------------------------
 update(Γ, f e, (f::t1->t2) (e::t1), t2) ⊢ f e ~ t2
```

Note that when `a` and `b` are applied to each other, we do this operation for *every* matching `t1 -> t2` in `types(a)`. We also error out if this would imply an ambiguous evaluation order.

Not only functions are callable. The expression `a b` attempts to evaluate to `@call a b` if `a b` fails.

```
 Γ ⊢ t0 -> t1 -> t2 ∈ types(@call) AND (a ~ t0 AND b ≅ t1)
-----------------------------------------------------------
                       Γ ⊢ a b ~ t2
```

Definitions of variables can be extended. Note that an expression appears on the right-hand side of the equation, but a *value* is what we are actually storing.

```
             Γ ⊢ e ~ t
---------------------------------------
 update(Γ, n, e::t, t) ⊢ (n:t = e) ~ t
```

We can string expressions together:

```
   Γ1 ⊢ a ~ t1 AND Γ2 ⊢ b ~ t2 
-----------------------------------
     Γ1 ∪ Γ2 ⊢ (a; b) ~ t2
```

One of the interesting things here is that it appears that we are forced to have lazy evaluation: because some expression `e` could have any number of different values, we can't evaluate it until the last minute. However, if we can determine precisely at compile time all evaluation paths (and we should, or else error), then we should be able to do compile to a strict evaluation model.
