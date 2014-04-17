object Expr =
  Num Int
  | Var Str
  | Apply Expr Expr
  | Lambda Str Expr
  | Block [Expr]
  with posn : Position

object Position with {
  line: Int;
  col: Int;
  source: Str
}

object CoolPosition <: Position = 
  CoolPosition (super: Position) <: super
  with {
    version: (Int, Int, Int)        
  }

trait Foo a =
  foo : a -> Str

impl Foo Position where
  foo p = "line #{p\line}, column #{p\col}"

impl Foo CoolPosition where
  foo cp = "This is a cool position #{cp\super.foo}, version #{cp\version}"

> p = Position with (line=1, col=5, source='my_doc.llm')
> println p\line
1
> println p\col * p\source.length
50
> foo = Var foo with posn = p
> println foo\p\col
5
> cp = CoolPosition p with version = (1,0,0)
> cp' = CoolPosition (Position with (line=0, col=2, source='bloop.txt')) with version = (1, 0, 0)
