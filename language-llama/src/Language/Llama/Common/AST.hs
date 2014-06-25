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
import Data.Set (Set, singleton, (\\))
import qualified Data.Set as S

import Language.Llama.Common.Common

data AbsExpr expr = Var          !Name
                  | Int          !Integer
                  | Float       !Double
                  | String       !Text
                  | InString     !(InString expr)
                  | Constructor  !Name
                  | Then         !expr !expr
                  | Dot          !expr !expr
                  | Apply        !expr !expr
                  | Binary       !Name !expr !expr
                  | Prefix       !Name !expr
                  | Postfix      !expr !Name
                  | Attribute    !expr !Name
                  | RefAttribute !expr !Name
                  | Lambda       !Name !expr
                  | Lambdas      ![(expr, expr)]
                  | Case         !expr ![(expr, expr)]
                  | MultiCase    !expr ![([expr], expr)]
                  | Tuple        ![expr] ![Kwarg expr]
                  | Literal      !(Literal expr)
                  | DeRef        !expr !expr
                  | Typed        !expr !Type
                  | If           !expr !expr !(Maybe expr)
                  | Unless       !expr !expr !(Maybe expr)
                  | PostIf       !expr !expr !(Maybe expr)
                  | PostUnless   !expr !expr !(Maybe expr)
                  | ForComp      !expr !(ForPattern expr)
                  | For          !(ForPattern expr) !expr
                  | PatternDef   !expr !expr
                  | Define       !Name !expr
                  | Return       !expr
                  | Throw        !expr
                  | TryCatch     !expr ![(expr, expr)] !(Maybe expr)
                  | Break        !expr
                  | After        !expr !expr
                  | Before       !expr !expr
                  | ObjDec       !(ObjectDec expr)
                  | Modified     !Text !expr
                  | TypeDef      !Name !Type
                  | LambdaDot    !expr ![expr]
                  | AssignDot    !expr !expr ![expr]
                  | With         !expr ![(Name, expr)]
                  | PatAssert    !(PatAssert expr)
                  | GetAttrib    !Name !Int !expr
                  | Continue
                  | WildCard
                  deriving (P.Show, Eq, Functor)

data Kwarg expr = Kwarg Name (Maybe Type) (Maybe expr)
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

instance Render e => Render (Kwarg e) where
  render (Kwarg name t e) = name <> t' <> e' where
    t' = case t of Nothing -> ""
                   Just typ -> ":" <> render typ
    e' = case e of Nothing -> ""
                   Just ex -> "=" <> render ex

instance (IsExpr e, Eq e, Render e) => Render (AbsExpr e)

rndr :: (IsExpr e, Render e) => Bool -> e -> Text
rndr isBlockStart e = case unExpr e of
  a `Then` b | isBlockStart -> "{" <> rndr False e
             | otherwise -> case unExpr b of
    _ `Then` _ -> rndr True a <> "; " <> rndr False b
    _ -> rndr True a <> "; " <> rndr False b <> "}"
  Var name -> name
  Constructor name -> name
  Int n -> render n
  Float n | isInt n -> render (floor n :: Int)
  Float n -> render n
  String s -> render s
  InString istr -> render istr
  Dot e1 e2 -> rec' e1 <> "." <> rec' e2
  After e1 e2 -> rec e1 <> " after " <> rec e2
  Apply e1 e2 -> rec' e1 <> " " <> rec' e2
  Binary op e1 e2 -> rec' e1 <> " " <> op <> " " <> rec' e2
  Prefix op e -> op <> rec' e
  Postfix e op -> rec' e <> op
  Literal l -> render l
  Tuple es kws -> "(" <> es' <> kws' <> ")" where
    es' = if length es == 1 then rec (head es) <> ","
          else T.intercalate ", " (map rec es)
    kws' = if length kws == 0 then ""
           else "; " <> T.intercalate ", " (map render kws)
  Lambda n e -> n <> " -> " <> rec e
  Lambdas alts -> T.intercalate " | " $ map go alts where
    go (a, b) = rec' a <> " -> " <> rec b
  DeRef e1 e2 -> rec e1 <> "[" <> rec e2 <> "]"
  PatternDef e1 e2 -> rec e1 <> " = " <> rec e2
  Define name e -> name <> " = " <> rec e
  Attribute e name -> rec' e <> "\\" <> name
  If cond t f -> _if "if" cond <> _thenelse t f
  Unless cond t f -> _if "unless" cond <> _thenelse t f
  PostIf e cond els -> rec' e <> " " <> _if "if" cond <> _else els
  PostUnless e cond els -> rec' e <> " " <> _if "unless" cond <> _else els
  Modified m e -> m <> " " <> rec e
  For for e -> case unExpr e of
    _ `Then` _ -> render for <> " " <> rndr True e
    _ -> render for <> " do " <> rec e
  PatAssert pa -> render pa
  Throw e -> "throw " <> rec e
  GetAttrib "" i e -> rec e <> "\\" <> render i
  GetAttrib name i e -> rec e <> "{" <> name <> "}\\" <> render i
  Case e alts -> "case " <> rec e <> " of " <> rndrOneAlts alts
  MultiCase e alts -> "case " <> rec e <> " of " <> rndrAlts alts
  _ -> show e
  where
    rec = rndr isBlockStart
    rndrAlt (pats, res) = do
      let pats' = T.intercalate " | " $ map (rndr True) pats
      pats' <> " -> " <> rndr True res
    rndrOneAlt (pat, res) = rndrAlt ([pat], res)
    rndrOneAlts alts = T.intercalate "; " $ map rndrOneAlt alts
    rndrAlts alts = T.intercalate "; " $ map rndrAlt alts
    _if kw c = kw <> " " <> rec c
    _thenelse t f = t' <> _else f where
      t' = case unExpr t of _ `Then` _ -> " " <> rndr True t
                            _ -> " then " <> rndr True t
    _else f = case f of {Nothing -> ""; Just e -> " else " <> rec e}
    -- | Wraps any non-primitive expressions in quotes.
    rec' expr = case unExpr expr of
      Var _ -> rec expr
      Constructor _ -> rec expr
      Int _ -> rec expr
      Float _ -> rec expr
      String _ -> rec expr
      InString _ -> rec expr
      Tuple _ _ -> rec expr
      Literal l -> rec expr
      _ -> "(" <> rec expr <> ")"

instance (IsExpr e, Render e) => Render (Literal e) where
  render = \case
    VecLiteral es -> "[" <> T.intercalate ", " (fmap rendr es) <> "]"
    VecRange e1 e2 -> "[" <> rendr e1 <> " .. " <> rendr e2 <> "]"
    DictLiteral [] -> "{=>}"
    DictLiteral es -> "{" <> T.intercalate ", " (fmap rpair es) <> "}"
    SetLiteral es -> "{" <> T.intercalate ", " (fmap rendr es) <> "}"
    JsonLiteral jl -> pack $ P.show jl
    where rpair (e1, e2) = rendr e1 <> " => " <> rendr e2
          rendr = rndr True

instance (IsExpr e, Render e) => Render (PatAssert e) where
  render = \case
    IsLiteral e1 e2 -> rendr e1 <> " === " <> rendr e2
    IsConstr name e -> "(" <> rendr e <> ")'s constructor is " <> name
    IsTupleOf n e -> "(" <> rendr e <> ") is tuple of length " <> render n
    IsVectorOf n e -> "(" <> rendr e <> ") is vector of length " <> render n
    IsArrayOf n e -> "(" <> rendr e <> ") is array of length " <> render n
    pa1 `And` pa2 -> render pa1 <> ", and " <> render pa2
    where rendr = rndr True

instance Render Expr' where
  render (Expr' e) = render e

instance (IsExpr e, Render e) => Render (ForPattern e) where
  render = \case
    Forever -> "forever"
    ForExpr e -> "for " <> rndr True e
    ForIn e1 e2 -> "for " <> rndr True e1 <> " in " <> rndr True e2
    For_ e1 e2 e3 -> do
      let [e1', e2', e3'] = map (rndr True) [e1, e2, e3]
      "for " <> e1' <> "; " <> e2' <> "; " <> e3'

instance Render e => Render (InString e) where
  render (Bare s) = render s
  render (InterpShow i1 e i2) = do
    let i1' = T.init $ T.tail $ render i1
    let i2' = T.init $ T.tail $ render i2
    let e' = render e
    "\"" <> i1' <> "#{" <> e' <> "}" <> i2' <> "\""

data BaseType
  = TVar Name
  | TConst Name
  | TApply BaseType BaseType
  deriving (P.Show, Eq, Ord)
type Map = HashMap
data Assertion = HasTrait Name [BaseType] deriving (P.Show, Eq, Ord)
newtype Context = Context (Set Assertion) deriving (P.Show, Eq)
data Type = Type Context BaseType deriving (P.Show, Eq)
data Polytype = Polytype (Set Name) Type deriving (P.Show, Eq)

instance Monoid Context where
  mempty = Context mempty
  mappend (Context c1) (Context c2) = Context (mappend c1 c2)

instance IsString BaseType where fromString = TConst . pack
instance IsString Type where fromString = Type mempty . fromString
instance IsString Polytype where fromString = Polytype mempty . fromString

instance Render BaseType where
  render t = case t of
    TVar name -> name
    TApply (TApply "->" t1) t2 -> render' t1 <> " -> " <> render t2
    TConst name -> name
    TApply t1 t2 -> renderP t1 <> " " <> renderP t2
    where render' t@(TApply (TApply "->" t1) t2) = renderP t
          render' t = render t
  renderP t@(TApply _ _ ) = "(" <> render t <> ")"
  renderP t = render t


instance Render Assertion where
  render (HasTrait name ts) =
    name <> " " <> T.intercalate " " (fmap renderP ts)

instance Render Context where
  render (Context ctx) | S.size ctx == 1 = rndr
                       | otherwise = "{" <> rndr <> "}"
    where rndr = T.intercalate ", " . fmap renderP . toList $ ctx

instance Render Type where
  render (Type ctx t) | ctx == mempty = render t
                      | otherwise = render ctx <> ". " <> render t
  renderP (Type ctx t)
    | ctx == mempty = render t
    | otherwise = "(" <> render ctx <> ". " <> render t <> ")"

symChars :: String
symChars = "><=+-*/^~!%&$:#|?_"

isSymbol :: Text -> Bool
isSymbol = T.all (`elem` symChars)

isPrefixSymbol :: Text -> Bool
isPrefixSymbol s = isSymbol s && T.last s == '_' && not (T.head s == '_')

isPostfixSymbol :: Text -> Bool
isPostfixSymbol s = isSymbol s && T.head s == '_' && not (T.last s == '_')

isInfixSymbol :: Text -> Bool
isInfixSymbol s = isSymbol s && T.head s == '_' && T.last s == '_'

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
