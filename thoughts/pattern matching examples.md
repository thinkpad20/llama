The following block:

```
try
  Just (blorp@(foo, bar)) = baz
  println bar / foo
  return blorp
catch PatternMatchError
  println 'wtf'
catch Exception msg
  println 'oh, something else: #[msg]'
```

Can be transformed into the below block:

```
try
  if (PatAssert $ (baz.ConstrIs 'Just').And $ (baz.Deref 'Just' 0).IsTupleOf 2)
    blorp = Deref baz 'Just' 0
    foo = blorp.derefTup 0
    bar = blorp.derefTup 1
    println bar/foo
    return blorp
  else
    throw PatternMatchError
catch _exc do
  if (PatAssert $ _exc.ConstrIs 'PatternMatchError')
    println 'wtf'
  else if (PatAssert $ _exc.ConstrIs 'Exception')
    msg = _exc.Deref 'Exception' 0
    println 'oh, something else: ' <> msg.show
  else
    throw _exc
```

Where we've added the following expression constructors:

```
data Expr = ...
          | PatAssert PatAssert
          | Deref Expr Name Int

data PatAssert = IsLiteral Expr Expr
               | ConstrIs Expr Name
               | IsTupleOf Expr Int
               | IsVectorOf Expr Int
               | PatAssert `And` PatAssert
               | PatAssert `Or` PatAssert
```

```
eval :: Expr -> Eval Value
eval expression = case expression of
  ...
  Deref expr constrName idx -> eval expr >>= evalDeref constrName idx
  PatAssert pa -> VBool <$> evalPatAssert pa

evalDeref :: Name -> Int -> Value -> Eval Value
evalDeref constrName idx val = case val of
  VInstance inst | constrName == instName inst -> return $ indexOf inst idx
                 | otherwise -> case instParent inst of
                   Nothing -> 
                     throwError "instance's constructor name doesn't match"
                   Just p -> evalDeref p constrName idx
  VTuple vals | length vals > idx -> getIndex vals idx
              | otherwise -> throwError "index out of range on tuple"
  VVector vals | length vals > idx -> getIndex vals idx
               | otherwise -> throwError "index out of range on vector"
  VMaybe (Just val) | idx == 0 -> return val
  VNum _ -> throwError "Number can't be dereferenced"
  ...

evalPatAssert :: PatAssert -> Eval Bool
evalPatAssert (ConstrIs expr name) = eval expr >>= \case
  VInstance inst | name == instName inst -> return True
  VMaybe (Just _) | name == "Just" -> return True
  VMaybe Nothing | name == "Nothing" -> return True
  _ -> return False
evalPatAssert (e1 `IsLiteral` e2) = do
  (v1, v2) <- (,) <$> eval e1 <*> eval e2
  isEqual v1 v2
evalPatAssert (IsTupleOf expr size) = eval expr >>= \case
  VTuple tup | length tup == size -> return True
  _ -> return False
evalPatAssert (IsVectorOf expr size) = eval expr >>= \case
  VVector vec | length vec == size -> return True
  _ -> return False
evalPatAssert (pa1 `And` pa2) = 
  fmap (&&) evalPatAssert pa1 <*> evalPatAssert pa2
evalPatAssert (pa1 `Or` pa2) = 
  fmap (||) evalPatAssert pa1 <*> evalPatAssert pa2

-- A more powerful version of (==), because it operates in the IO monad.
isEqual :: Value -> Value -> Eval Value
isEqual = undefined
```

Note: the above `evalPatAssert` seems that it would get the job done, but is inefficient, because we're evaluating expressions more than once, and dangerous, because those evaluations could have side effects. We need to think about this critically. Perhaps we evaluate the whole thing? But then how would we express the nested structure? Need to thinking about this. I don't think the answer needs to be complicated.
