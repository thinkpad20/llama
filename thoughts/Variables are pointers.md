Variables are pointers (IORefs) to Values.
References are pointers that have been shared.

So when we say `a = mut 1`, we

* allocate space for an int at address X
* put 1 at address X
* allocate space for a pointer at address Y
* put X at address Y
* in the symbol table, point `a` at Y

Then let's say we next say `b = ref a`. That means we

* allocate space for a pointer at address Z
* put X at address Z
* in the symbol table, point `b` at Z

So now, both a and b are effectively the same, pointing at the same space.
If we instead just had `a = 1`, then we just map `a` to `1` in the symbol table.

```haskell
eval (Modified Mut expr) = do
  val <- eval expr
  VRef <$> lift2 (newIORef val)
eval (Modified Ref expr) = eval expr >>= \case
  VRef ref -> return (VRef ref)
  val -> VRef <$> lift2 (newIORef val)
eval (Define name expr) = store name =<< eval expr
```
