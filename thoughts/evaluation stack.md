evaluation stack...

```haskell
data EvalState = ES {
    esStack = [Frame]
  , esEnv = 
  }
```

start off with just builtins, then imported functions (down the road)...


you have a stack-based evaluator... sounds good right?

```
foo n = n.negate
println foo 3
```

Assume for simplicity that `negate` and `println` only work on numbers, and they're builtin functions. Then how would this get evaluated?

```
eval (Block [foo = ..., println ...])
 => eval (Define "foo" (Lambda ...))
  => result <- eval (Lambda (Var "n") (Dot (Var "n") (Var "negate"))
```

Lambdas get evaluated into closures. So we need to create a closure that records that `n` is the argument. We should be creating something like 

```
result <- Closure (Dot (Var "n") (Var "negate")) {"n" => Local Arg}
```

Then we'd store this under the name `foo`:

```
  => store "foo" result
```

Storing would put `foo` in the current stack, so now our env contains

```
[arg=(), env={foo: Closure .....}, env={println: Builtin (...), negate: Builtin (...)}]
```

So then we'd go to the next line and evaluate that

```
  => eval (Apply (Var "println") (Apply (Var "foo") (Number 3)))
    => eval (Var "println")
```

So the first thing we do is look up `println` in our environment. We look in the first env, and it's not there, so we look one level down, and there's println, yay. Then we need to evaluate the right side, which means we need to evaluate `Apply (Var "foo") (Number 3)`.

....

Alright so what if we capture variables? ... let's wait for this...
