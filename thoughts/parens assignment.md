I don't think we need to have parens around when we are pattern matching with a tuple.

If we want to be able to do this:

```
foo n = (n + 1, n * 12, (3 / n) ^ 4)
bar, baz, qux = foo 10
```

This parser should do the trick:

```
pPatternDef = do
  patterns <- pExpr `sepBy1` schar ','
  exactSym "="
  expr <- pExprOrBlock
  case patterns of
    [p] -> PatternDef p expr
    ps  -> PatternDef (tuple ps) expr
```
