{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module AST where

import qualified Prelude as P
import Data.Text (intercalate, replicate)
import qualified Data.Text as T
import Data.String
import qualified Data.Map as M

import Common hiding (intercalate)


type Kwargs = [(Name, Either Expr Type)]
data Expr = Var         !Name
          | Number      !Double
          | String      !Text
          | InString    !InString
          | Constructor !Name
          | Block       !Block
          | Dot         !Expr !Expr
          | Apply       !Expr !Expr
          | Attribute   !Expr !Name
          | Lambda      !Expr !Expr
          | Lambdas     ![(Expr, Expr)]
          | Case        !Expr ![(Expr, Expr)]
          | MultiCase   !Expr ![([Expr], Expr)]
          | Tuple       ![Expr] Kwargs
          | Literal     !Literal
          | DeRef       !Expr !Expr
          | Typed       !Expr !Type
          | If          !Expr !Expr !Expr
          | If'         !Expr !Expr
          | For         !Expr !Expr !Expr !Expr
          | ForIn       !Expr !Expr !Expr
          | Forever     !Expr
          | PatternDef  !Expr !Expr
          | Define      !Name !Expr
          | Extend      !Name !Expr
          | Assign      !Expr !Expr
          | Return      !Expr
          | Throw       !Expr
          | TryCatch    !Expr ![(Expr, Expr)] !(Maybe Expr)
          | Break       !Expr
          | After       !Expr !Expr
          | Before      !Expr !Expr
          | ObjDec      !ObjectDec
          | Modified    !Mod !Expr
          | TypeDef     !Name !Type
          | Prefix      !Name !Expr
          | LambdaDot   !Expr
          | AssignOp    !Name !Expr !Expr
          | With        !Expr ![(Name, Expr)]
          | PatAssert   !PatAssert
          | Deref       !Name !Int !Expr
          | Continue
          | WildCard
          deriving (P.Show, Eq)

data PatAssert = IsLiteral !Expr !Expr
               | IsConstr !Name !Expr
               | IsTupleOf !Int !Expr
               | IsVectorOf !Int !Expr
               | IsArrayOf !Int !Expr
               | !PatAssert `And` !PatAssert
               deriving (P.Show, Eq)

type TKwargs = [(Name, Type)]
data Type = TVar       !Name
          | TConst     !Name
          | TTuple     ![Type] !TKwargs
          | TApply     !Type !Type
          | TFunction  !Type !Type
          | TMod       !Mod !Type
          | TMultiFunc !TypeMap
          deriving (P.Show, Eq, Ord)

type Attribute = (Name, Type, Maybe Expr)

data Mod = Mut | Ref | Pure | Local | Lazy deriving (P.Show, Eq, Ord)
type TypeMap = M.Map Type Type

instance Render Type where
  render t = case t of
    TVar name -> name
    TConst name -> name
    TTuple ts _ -> "(" <> intercalate ", " (map render ts) <> ")"
    TApply (TConst "[]") typ -> "[" <> render typ <> "]"
    TApply (TConst "[!]") typ -> "[!" <> render typ <> "]"
    TApply a b -> render a <> " " <> render' b
    TFunction t1 t2 -> render'' t1 <> " -> " <> render t2
    TMod modi typ -> render modi <> " " <> render typ
    TMultiFunc tset -> "{" <> renderSet tset <> "}"
    where render' typ = case typ of
            TApply _ _ -> "(" <> render typ <> ")"
            _ -> render typ
          render'' typ = case typ of
            TFunction _ _ -> "(" <> render typ <> ")"
            _ -> render typ
          renderSet tset =
            let pairs = M.toList tset
                rPair (from, to) = render from <> " -> " <> render to
            in intercalate ", " (map rPair pairs)

instance Render [M.Map Name Type] where
  render mps = line $ "[" <> (intercalate ", " $ map render mps) <> "]"

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

data Literal = ArrayLiteral ![Expr]
             | ArrayRange   !Expr !Expr
             | DictLiteral  ![(Expr, Expr)]
             | SetLiteral   ![Expr]
             | ListLiteral  ![Expr]
             deriving (P.Show, Eq)

type Block = [Expr]

data ObjectDec = ObjectDec {
    objName :: Name
  , objExtends :: Maybe Name
  , objVars :: [Name]
  , objConstrs :: [ConstructorDec]
  , objAttrs :: [Expr]
  }
  deriving (P.Show, Eq)

defObj :: ObjectDec
defObj = ObjectDec {
    objName = "(some object)"
  , objExtends = Nothing
  , objVars = []
  , objConstrs = []
  , objAttrs = []
  }

data ConstructorDec = ConstructorDec {
    constrName :: Name
  , constrArgs :: [Expr]
  , constrExtends :: Maybe Expr
  , constrLogic :: Maybe Expr
  }
  deriving (P.Show, Eq)

defConstr :: ConstructorDec
defConstr = ConstructorDec {
    constrName = "(some constructor)"
  , constrArgs = []
  , constrExtends = Nothing
  , constrLogic = Nothing
  }

data InString = Plain Text
              | InterpShow InString Expr InString
              | Interp InString Expr InString
              deriving (P.Show, Eq)

instance IsString InString where
  fromString str = Plain $ pack str

instance Monoid InString where
  mempty = Plain mempty
  is1 `mappend` is2 = case (is1, is2) of
    (Plain s, Plain s') -> Plain (s <> s')
    (s, InterpShow is e is') -> InterpShow (s <> is) e is'
    (InterpShow is e is', s) -> InterpShow is e (is' <> s)
    (s, Interp is e is') -> Interp (s <> is) e is'
    (Interp is e is', s) -> Interp is e (is' <> s)

instance Render Expr where
  render e = evalState (render' e) 0 where
    render' :: Expr -> State Int Text
    render' expr = case expr of
      Var name -> return name
      Constructor name -> return name
      Number n | isInt n -> return $ render $ (floor n :: Int)
      Number n -> return $ render n
      Block blk -> block blk
      String s -> return $ render s
      Dot e1 e2 -> return $ render'' e1 <> "." <> render'' e2
      Apply (Var op) (Tuple [e1, e2] _)
        | isSymbol op -> return $ render'' e1 <> " " <> op <> " " <> render'' e2
      Apply (Var op) ex | isSymbol op -> return $ op <> " " <> render'' ex
      Apply e1 e2 -> return $ render'' e1 <> " " <> render'' e2
      Tuple es _ -> return $ "(" <> (intercalate "," . map render) es <> ")"
      Literal (ArrayLiteral exprs) -> return $ "[" <> intercalate ", " (map render exprs) <> "]"
      Literal (ArrayRange start stop) -> return $ "[" <> render start <> ".." <> render stop <> "]"
      DeRef object idx -> return $ render'' object <> "[:" <> render idx <> "]"
      Lambda arg ex -> return $ render'' arg <> " => " <> render ex
      Case ex alts -> return $ "case " <> render ex <> " of " <> intercalate " | " rPairs
        where rPairs = map (\(p, r) -> render p <> " => " <> render r) alts
      Typed ex typ -> return $ render'' ex <> ": " <> render typ
      If c t f -> do
        if' <- mkLine $ "if " <> render c
        else' <- mkLine "else"
        t' <- render' t
        f' <- render' f
        join [if', t', else', f']
      If' c t -> do
        if' <- mkLine $ "if " <> render c
        t' <- render' t
        join [if', t']
      For start cond step blk -> do
        for <- mkLine $ "for " <> render start <> "; "
                       <> render cond <> "; " <> render step
        blk' <- render' blk
        join [for, blk']
      ForIn pat ex blk -> do
        for <- mkLine $ "for " <> render pat <> " in " <> render ex
        blk' <- render' blk
        join [for, blk']
      Define name ex -> mkLine $ name <> " = " <> render ex
      Extend name ex -> mkLine $ name <> " &= " <> render ex
      Assign e1 e2 -> mkLine $ render e1 <> " := " <> render e2
      Break ex -> mkLine $ "break " <> render ex
      Throw ex -> mkLine $ "throw " <> render ex
      Return ex -> mkLine $ "return " <> render ex
      TypeDef name typ -> mkLine $ "typedef " <> name <> " = " <> render typ
      _ -> return $ render expr
    mkLine str = get >>= \i -> return $ replicate i " " <> str
    join = return . intercalate "\n"
    block blk = up *> fmap (intercalate "; ") (mapM render' blk) <* down
    (up, down) = (modify (+2), modify (\n -> n - 2))
    render'' expr = case expr of
      Apply _ _ -> parens
      Dot _ _ -> parens
      Lambda _ _ -> parens
      _ -> render expr
      where parens = "(" <> render expr <> ")"

instance Render Block where
  render b = "{" <> (line . trim . intercalate "; " . map render) b <> "}"

symChars :: String
symChars = "><=+-*/^~!%&$:#|?"

isSymbol :: Text -> Bool
isSymbol = T.all (`elem` symChars)

binary :: Name -> Expr -> Expr -> Expr
binary name e1 e2 = Apply (Apply (Var name) e1) e2

tuple :: [Expr] -> Expr
tuple exprs = Tuple exprs mempty
unit :: Expr
unit = tuple []

just :: Expr -> Expr
just = Apply (Constructor "Just")

nothing :: Expr
nothing = Constructor "Nothing"

true, false :: Expr
true = Constructor "True"
false = Constructor "False"

instance Monoid Expr where
  mempty = Block []
  Block b1 `mappend` Block b2 = Block (b1 <> b2)
  Block b `mappend` e = Block (b <> [e])
  e `mappend` Block b = Block (e:b)
  e1 `mappend` e2 = Block [e1, e2]
