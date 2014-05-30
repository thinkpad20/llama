Assign a unique ID to each expression during parsing, which can be stored and later used to look up the expression:

```haskell
type SourcePos = (Int, Int)
type NodeId = Int
data Expr = Expr SourcePos NodeId ExprNode
data ExprNode
  = Var Name
  | Num Double
  | Apply Expr Expr
  | Let Name Expr Expr
  | Lambda Name Expr

data DesugaredExpr = DExpr Expr DExprNode
data DExprNode
  = DVar Name
  | DNum Double
  | DApply DesugaredExpr DesugaredExpr
  | DLambda Name DesugaredExpr

desugar :: Expr -> DesugaredExpr
desugar e = DExpr e $ case e of
  Var n -> DVar n
  Num d -> DNum d
  Apply e1 e2 -> DApply e1 e2
  Lambda n e -> DLambda n e
  Let n e1 e2 -> DApply (DExpr e (DLambda n e2)) e1
```

```rust
type SourcePos = (u32, u32);

struct Expr {
  node: ExprNode,
  id: NodeId,
  pos: SourcePos
}

enum ExprNode {
  Var(Name),
  Num(Num),
  Apply(Box<Expr>, Box<Expr>),
  Lambda(Name, Box<Expr>),
  Let(Name, Box<Expr>, Box<Expr>)
}

struct DeugaredExpr {
  orig: Expr,
  node: DExprNode
}

enum DExprNode {
  DVar(Name),
  DNum(Num),
  DApply(Box<DesugaredExpr>, Box<DesugaredExpr>),
  DLambda(Name, Box<DesugaredExpr>)
}
```
