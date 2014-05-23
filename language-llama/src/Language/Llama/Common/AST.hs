{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Language.Llama.Common.AST where

import qualified Prelude as P
import qualified Data.Map as M
import qualified Data.Text as T

import Language.Llama.Common.Common

type Kwargs expr = [(Name, Either Type expr)]
data AbsExpr expr = Var          !Name
                  | Number       !Double
                  | String       !Text
                  | InString     !(InString expr)
                  | Constructor  !Name
                  | Block        ![expr]
                  | Dot          !expr !expr
                  | Apply        !expr !expr
                  | Binary       !Name !expr !expr
                  | Unary        !Name !expr
                  | Attribute    !expr !Name
                  | RefAttribute !expr !Name
                  | Lambda       !expr !expr
                  | Lambdas      ![(expr, expr)]
                  | Case         !expr ![(expr, expr)]
                  | MultiCase    !expr ![([expr], expr)]
                  | Tuple        ![expr] (Kwargs expr)
                  | Literal      !(Literal expr)
                  | DeRef        !expr !expr
                  | Typed        !expr !Type
                  | If           !expr !expr !(Maybe expr)
                  | Unless       !expr !expr !(Maybe expr)
                  | PostIf       !expr !expr
                  | PostUnless   !expr !expr
                  | ForComp      !expr !(ForPattern expr)
                  | For          !(ForPattern expr) !expr
                  | PatternDef   !expr !expr
                  | Define       !Name !expr
                  | Extend       !Name !expr
                  | Assign       !expr !expr
                  | Return       !expr
                  | Throw        !expr
                  | TryCatch     !expr ![(expr, expr)] !(Maybe expr)
                  | Break        !expr
                  | After        !expr !expr
                  | Before       !expr !expr
                  | ObjDec       !(ObjectDec expr)
                  | Modified     !Text !expr
                  | TypeDef      !Name !Type
                  | Prefix       !Name !expr
                  | LambdaDot    !expr
                  | AssignOp     !Name !expr !expr
                  | With         !expr ![(Name, expr)]
                  | PatAssert    !(PatAssert expr)
                  | GetAttrib    !Name !Int !expr
                  | Continue
                  | WildCard
                  deriving (P.Show, Eq, Functor)

data ForPattern e = Forever       -- forever do ...
                  | ForExpr !e    -- for 10.range do ...
                  | ForIn !e !e   -- for x in list do ...
                  | For_ !e !e !e -- for i = 0; i < 10; i.incr! do ...
                  deriving (P.Show, Eq, Functor)

data PatAssert expr = IsLiteral !expr !expr
                    | IsConstr !Name !expr
                    | IsTupleOf !Int !expr
                    | IsVectorOf !Int !expr
                    | IsArrayOf !Int !expr
                    | !(PatAssert expr) `And` !(PatAssert expr)
                    deriving (P.Show, Eq, Functor)


data Literal expr = VecLiteral      ![expr]
                  | VecRange        !expr !expr
                  | DictLiteral     ![(expr, expr)]
                  | SetLiteral      ![expr]
                  | JsonLiteral     !JLiteral
                  deriving (P.Show, Eq, Functor)

data JLiteral = JString Text
              | JNum Double
              | JBool Bool
              | JNull
              | JArray [JLiteral]
              | JObject [(Name, JLiteral)]
              deriving (P.Show, Eq)

data ObjectDec expr = ObjectDec
  { objName :: Name
  , objExtends :: Maybe Name
  , objVars :: [Name]
  , objConstrs :: [ConstructorDec expr]
  , objAttrs :: [expr] }
  deriving (P.Show, Eq, Functor)

data ConstructorDec expr = ConstructorDec
  { constrName :: Name
  , constrArgs :: [expr]
  , constrExtends :: Maybe expr }
  deriving (P.Show, Eq, Functor)

-- | An interpolated string.
data InString e  = Bare Text
                 | InterpShow (InString e) e (InString e)
                 | Interp (InString e) e (InString e)
                 deriving (P.Show, Eq, Functor)

instance IsString (InString e) where fromString = Bare . pack

newtype Expr' = Expr' (AbsExpr Expr') deriving (P.Show, Eq)

class IsExpr e where
  unExpr :: e -> AbsExpr e
  bareExpr :: e -> Expr'
  bareExpr e = Expr' $ fmap bareExpr $ unExpr e

instance IsExpr Expr' where
  unExpr (Expr' e) = e
  bareExpr = P.id

instance Monoid (InString e) where
  mempty = Bare mempty
  is1 `mappend` is2 = case (is1, is2) of
    (Bare s, Bare s') -> Bare (s <> s')
    (s, InterpShow is e is') -> InterpShow (s <> is) e is'
    (InterpShow is e is', s) -> InterpShow is e (is' <> s)
    (s, Interp is e is') -> Interp (s <> is) e is'
    (Interp is e is', s) -> Interp is e (is' <> s)

instance (IsExpr e, Eq e, Render e) => Render (AbsExpr e) where
  render e = case e of
    Var name -> name
    Constructor name -> name
    Number n | isInt n -> render (floor n :: Int)
    Number n -> render n
    Block es -> "{" <> T.intercalate "; " (map render es) <> "}"
    String s -> render s
    Dot e1 e2 -> render' e1 <> "." <> render' e2
    Apply e1 e2 -> render' e1 <> " " <> render' e2
    Binary op e1 e2 -> render' e1 <> " " <> op <> " " <> render' e2
    Unary op e -> op <> render' e
    Tuple es kws | kws == mempty -> "(" <> es' <> ")" where
      es' = if length es == 1 then render (head es) <> ","
            else T.intercalate ", " (map render es)

    Lambda a b -> render a <> " => " <> render b
    DeRef e1 e2 -> render e1 <> "[" <> render e2 <> "]"
    InString istr -> render istr
    PatternDef e1 e2 -> render e1 <> " = " <> render e2
    Assign e1 e2 -> render e1 <> " := " <> render e2
    Attribute e name -> render' e <> "\\" <> name
    If cond t f -> _if "if" cond <> _thenelse t f
    Unless cond t f -> _if "unless" cond <> _thenelse t f
    PostIf e cond -> render e <> " " <> _if "if" cond
    PostUnless e cond -> render e <> " " <> _if "unless" cond
    For for e -> case unExpr e of
      Block _ -> render for <> " " <> render e
      _ -> render for <> " do " <> render e
    _ -> show e
    where
      _if kw c = kw <> " " <> render c
      _thenelse t f = t' <> f' where
        f' = case f of {Nothing -> ""; Just e -> " else " <> render e}
        t' = case unExpr t of Block _ -> " " <> render t
                              _ -> " then " <> render t
      render' expr = case unExpr expr of
        Apply _ _ -> parens
        Dot _ _ -> parens
        Lambda _ _ -> parens
        Binary _ _ _ -> parens
        Unary _ _ -> parens
        Assign _ _ -> parens
        _ -> render expr
        where parens = "(" <> render expr <> ")"

instance Render Expr' where
  render (Expr' e) = render e

instance Render e => Render (ForPattern e) where
  render = \case
    Forever -> "forever"
    ForExpr e -> "for " <> render e
    ForIn e1 e2 -> "for " <> render e1 <> " in " <> render e2
    For_ e1 e2 e3 -> do
      let [e1', e2', e3'] = map render [e1, e2, e3]
      "for " <> e1' <> "; " <> e2' <> "; " <> e3'

instance Render e => Render (InString e) where
  render (Bare s) = render s
  render (InterpShow i1 e i2) = do
    let i1' = T.init $ T.tail $ render i1
    let i2' = T.init $ T.tail $ render i2
    let e' = render e
    "\"" <> i1' <> "#{" <> e' <> "}" <> i2' <> "\""

type TKwargs = [(Name, Type)]
data Type = TVar       !Name
          | TConst     !Name
          | TTuple     ![Type] !TKwargs
          | TApply     !Type !Type
          | TFunction  !Type !Type
          | TMod       !Text !Type
          | TMultiFunc !TypeMap
          deriving (P.Show, Eq, Ord)

type TypeMap = M.Map Type Type

instance Render Type where
   render t = case t of
     TVar name -> name
     TConst name -> name
     TTuple ts _ -> "(" <> T.intercalate ", " (map render ts) <> ")"
     TApply (TConst "[]") typ -> "[" <> render typ <> "]"
     TApply (TConst "[!]") typ -> "[!" <> render typ <> "]"
     TApply a b -> render a <> " " <> render' b
     TFunction t1 t2 -> render'' t1 <> " -> " <> render t2
     TMod modi typ -> modi <> " " <> render typ
     TMultiFunc tset -> "{" <> renderSet tset <> "}"
     where render' typ = case typ of
             TApply _ _ -> "(" <> render typ <> ")"
             _ -> render typ
           render'' typ = case typ of
             TFunction _ _ -> "(" <> render typ <> ")"
             _ -> render typ
           renderSet tset = do
             let pairs = M.toList tset
                 rPair (from, to) = render from <> " -> " <> render to
             T.intercalate ", " (map rPair pairs)

instance Monoid Type where
  mempty = TMultiFunc mempty
  TMultiFunc s `mappend` TMultiFunc s' = TMultiFunc $ M.union s s'
  TMultiFunc s `mappend` TFunction from to = TMultiFunc $ M.insert from to s
  TFunction from to `mappend` TMultiFunc s = TMultiFunc $ M.insert from to s
  TFunction f1 t1 `mappend` TFunction f2 t2 =
    TMultiFunc $ M.fromList [(f1, t1), (f2, t2)]
  t1 `mappend` t2 = P.error $ "Invalid <>s: " <> P.show t1 <> ", " <> P.show t2

symChars :: String
symChars = "><=+-*/^~!%&$:#|?"

isSymbol :: Text -> Bool
isSymbol = T.all (`elem` symChars)

defConstr :: ConstructorDec e
defConstr = ConstructorDec
  { constrName = "(some constructor)"
  , constrArgs = []
  , constrExtends = Nothing }

defObj :: ObjectDec e
defObj = ObjectDec
  { objName = "(some object)"
  , objExtends = Nothing
  , objVars = []
  , objConstrs = []
  , objAttrs = [] }
