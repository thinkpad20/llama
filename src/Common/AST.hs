{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module AST where

import qualified Prelude as P
import qualified Data.Map as M
import qualified Data.Text as T

import Common

type Kwargs expr = [(Name, Either Type expr)]
data AbsExpr expr = Var         !Name
                  | Number      !Double
                  | String      !Text
                  | InString    !(InString expr)
                  | Constructor !Name
                  | Block       ![expr]
                  | Dot         !expr !expr
                  | Apply       !expr !expr
                  | Binary      !Name !expr !expr
                  | Attribute   !expr !Name
                  | Lambda      !expr !expr
                  | Lambdas     ![(expr, expr)]
                  | Case        !expr ![(expr, expr)]
                  | MultiCase   !expr ![([expr], expr)]
                  | Tuple       ![expr] (Kwargs expr)
                  | Literal     !(Literal expr)
                  | DeRef       !expr !expr
                  | Typed       !expr !Type
                  | If          !expr !expr !(Maybe expr)
                  | For         !expr !expr !expr !expr
                  | ForIn       !expr !expr !expr
                  | Forever     !expr
                  | PatternDef  !expr !expr
                  | Define      !Name !expr
                  | Extend      !Name !expr
                  | Assign      !expr !expr
                  | Return      !expr
                  | Throw       !expr
                  | TryCatch    !expr ![(expr, expr)] !(Maybe expr)
                  | Break       !expr
                  | After       !expr !expr
                  | Before      !expr !expr
                  | ObjDec      !(ObjectDec expr)
                  | Modified    !Mod !expr
                  | TypeDef     !Name !Type
                  | Prefix      !Name !expr
                  | LambdaDot   !expr
                  | AssignOp    !Name !expr !expr
                  | With        !expr ![(Name, expr)]
                  | PatAssert   !(PatAssert expr)
                  | GetAttrib   !Name !Int !expr
                  | Continue
                  | WildCard
                  deriving (P.Show, Eq, Functor)

data PatAssert expr = IsLiteral !expr !expr
                    | IsConstr !Name !expr
                    | IsTupleOf !Int !expr
                    | IsVectorOf !Int !expr
                    | IsArrayOf !Int !expr
                    | !(PatAssert expr) `And` !(PatAssert expr)
                    deriving (P.Show, Eq, Functor)

type TKwargs = [(Name, Type)]
data Type = TVar       !Name
          | TConst     !Name
          | TTuple     ![Type] !TKwargs
          | TApply     !Type !Type
          | TFunction  !Type !Type
          | TMod       !Mod !Type
          | TMultiFunc !TypeMap
          deriving (P.Show, Eq, Ord)

type Attribute expr = (Name, Type, Maybe expr)

data Mod = Mut | Ref | Pure | Local | Lazy deriving (P.Show, Eq, Ord)
type TypeMap = M.Map Type Type

instance Render Type -- where
  -- render t = case t of
  --   TVar name -> name
  --   TConst name -> name
  --   TTuple ts _ -> "(" <> intercalate ", " (map render ts) <> ")"
  --   TApply (TConst "[]") typ -> "[" <> render typ <> "]"
  --   TApply (TConst "[!]") typ -> "[!" <> render typ <> "]"
  --   TApply a b -> render a <> " " <> render' b
  --   TFunction t1 t2 -> render'' t1 <> " -> " <> render t2
  --   TMod modi typ -> render modi <> " " <> render typ
  --   TMultiFunc tset -> "{" <> renderSet tset <> "}"
  --   where render' typ = case typ of
  --           TApply _ _ -> "(" <> render typ <> ")"
  --           _ -> render typ
  --         render'' typ = case typ of
  --           TFunction _ _ -> "(" <> render typ <> ")"
  --           _ -> render typ
  --         renderSet tset =
  --           let pairs = M.toList tset
  --               rPair (from, to) = render from <> " -> " <> render to
  --           in intercalate ", " (map rPair pairs)

-- instance Render [M.Map Name Type] where
--   render mps = line $ "[" <> (intercalate ", " $ map render mps) <> "]"

instance Monoid Type where
  mempty = TMultiFunc mempty
  TMultiFunc s `mappend` TMultiFunc s' = TMultiFunc $ M.union s s'
  TMultiFunc s `mappend` TFunction from to = TMultiFunc $ M.insert from to s
  TFunction from to `mappend` TMultiFunc s = TMultiFunc $ M.insert from to s
  TFunction f1 t1 `mappend` TFunction f2 t2 =
    TMultiFunc $ M.fromList [(f1, t1), (f2, t2)]
  t1 `mappend` t2 = P.error $ "Invalid <>s: " <> P.show t1 <> ", " <> P.show t2

instance Render Mod where
  render Mut = "mut"
  render Ref = "ref"
  render Pure = "pure"
  render Local = "local"
  render Lazy = "lazy"

data Literal expr = ArrayLiteral ![expr]
                  | ArrayRange   !expr !expr
                  | DictLiteral  ![(expr, expr)]
                  | SetLiteral   ![expr]
                  | ListLiteral  ![expr]
                  deriving (P.Show, Eq, Functor)

data ObjectDec expr = ObjectDec {
    objName :: Name
  , objExtends :: Maybe Name
  , objVars :: [Name]
  , objConstrs :: [ConstructorDec expr]
  , objAttrs :: [expr]
  }
  deriving (P.Show, Eq, Functor)

defObj :: ObjectDec e
defObj = ObjectDec {
    objName = "(some object)"
  , objExtends = Nothing
  , objVars = []
  , objConstrs = []
  , objAttrs = []
  }

data ConstructorDec expr = ConstructorDec {
    constrName :: Name
  , constrArgs :: [expr]
  , constrExtends :: Maybe expr
  , constrLogic :: Maybe expr
  }
  deriving (P.Show, Eq, Functor)

defConstr :: ConstructorDec e
defConstr = ConstructorDec {
    constrName = "(some constructor)"
  , constrArgs = []
  , constrExtends = Nothing
  , constrLogic = Nothing
  }

data InString e  = Plain Text
                 | InterpShow (InString e) e (InString e)
                 | Interp (InString e) e (InString e)
                 deriving (P.Show, Eq, Functor)

instance IsString (InString e) where
  fromString str = Plain $ pack str

newtype Expr' = Expr' (AbsExpr Expr') deriving (Show, Eq)

class IsExpr e where
  unExpr :: e -> AbsExpr e
  bareExpr :: e -> Expr'
  bareExpr e = Expr' $ fmap bareExpr $ unExpr e

instance IsExpr Expr' where
  unExpr (Expr' e) = e
  bareExpr = P.id

instance Monoid (InString e) where
  mempty = Plain mempty
  is1 `mappend` is2 = case (is1, is2) of
    (Plain s, Plain s') -> Plain (s <> s')
    (s, InterpShow is e is') -> InterpShow (s <> is) e is'
    (InterpShow is e is', s) -> InterpShow is e (is' <> s)
    (s, Interp is e is') -> Interp (s <> is) e is'
    (Interp is e is', s) -> Interp is e (is' <> s)

instance Render Expr' where
  render e = case unExpr e of
    Var name -> name
    Constructor name -> name
    Number n | isInt n -> render (floor n :: Int)
    Number n -> render n
    Block es -> "{" <> T.intercalate "; " (map render es) <> "}"
    String s -> render s
    Dot e1 e2 -> render' e1 <> "." <> render' e2
    Apply e1 e2 -> render' e1 <> " " <> render' e2
    Binary op e1 e2 -> render' e1 <> " " <> op <> " " <> render' e2
    Tuple es kws | kws == mempty ->
      "(" <> T.intercalate ", " (map render es) <> ")"
    Lambda a b -> render a <> " => " <> render b
    _ -> pack $ show e
    where
      rec = render . Expr'
      render' expr = case unExpr expr of
        Apply _ _ -> parens
        Dot _ _ -> parens
        Lambda _ _ -> parens
        Binary _ _ _ -> parens
        _ -> render expr
        where parens = "(" <> render expr <> ")"

symChars :: String
symChars = "><=+-*/^~!%&$:#|?"

isSymbol :: Text -> Bool
isSymbol = T.all (`elem` symChars)
--
-- instance Monoid (AbsExpr e) where
--   mempty = Block []
--   Block b1 `mappend` Block b2 = Block (b1 <> b2)
--   Block b `mappend` e = Block (b <> [e])
--   e `mappend` Block b = Block (e:b)
--   e1 `mappend` e2 = Block [e1, e2]
