{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module AST where

import Common
import qualified Data.Text as T
import qualified Data.Map as M

data Expr = Var         !Name
          | Number      !Double
          | String      !T.Text
          | Constructor !Name
          | Block       !Block
          | Dot         !Expr !Expr
          | Apply       !Expr !Expr
          | Unary       !String !Expr
          | Lambda      !Expr !Expr
          | Lambdas     ![(Expr, Expr)]
          | Case        !Expr ![(Expr, Expr)]
          | Tuple       ![Expr]
          | Array       !ArrayLiteral
          | Ref         !Expr !Expr
          | Typed       !Expr !Type
          | If          !Expr !Expr !Expr
          | If'         !Expr !Expr
          | While       !Expr !Expr
          | For         !Expr !Expr !Expr
          | Define      !Name !Expr
          | Extend      !Name !Expr
          | Assign      !Expr !Expr
          | Return      !Expr
          | Throw       !Expr
          | Break       !Expr
          | After       !Expr !Expr
          | Before      !Expr !Expr
          | Continue
          | Mut         !Expr
          | TypeDef !Name !Type
          deriving (Show, Eq)

-- | A variable can be rigid (fixed in scope), or polymorphic (free to take on
-- multiple forms in the same scope).
data Type = TRigidVar  !Name
          | TPolyVar   !Name
          | TConst     !Name
          | TTuple     ![Type]
          | TApply     !Type !Type
          | TFunction  !Type !Type
          | TMut       !Type
          | TMultiFunc !(M.Map Type Type)
          deriving (Show, Eq, Ord)

data ArrayLiteral = ArrayLiteral ![Expr]
                  | ArrayRange !Expr !Expr deriving (Show, Eq)

type Block = [Expr]

instance Monoid Type where
  mempty = TMultiFunc mempty
  TMultiFunc s `mappend` TMultiFunc s' = TMultiFunc $ M.union s s'
  TMultiFunc s `mappend` TFunction from to = TMultiFunc $ M.insert from to s
  TFunction from to `mappend` TMultiFunc s = TMultiFunc $ M.insert from to s
  TFunction f1 t1 `mappend` TFunction f2 t2 =
    TMultiFunc $ M.fromList [(f1, t1), (f2, t2)]
  t1 `mappend` t2 = error $ "Invalid `or`s: " <> show t1 <> ", " <> show t2

instance Render Expr where
  render e = evalState (render' e) 0 where
    render' :: Expr -> State Int T.Text
    render' stmt = case stmt of
      Var name -> return name
      Constructor name -> return name
      Number n | isInt n -> return $ T.pack $ show $ floor n
      Number n -> return $ T.pack $ show n
      Block blk -> block blk
      String s -> return $ T.pack $ show s
      Dot e1 e2 -> return $ render'' e1 <> "." <> render'' e2
      Apply (Var op) (Tuple [e1, e2])
        | isSymbol op -> return $ render'' e1 <> " " <> op <> " " <> render'' e2
      Apply (Var op) e | isSymbol op -> return $ op <> " " <> render'' e
      Apply e1 e2 -> return $ render'' e1 <> " " <> render'' e2
      Tuple es -> return $ "(" <> (T.intercalate "," . map render) es <> ")"
      Array (ArrayLiteral exprs) -> return $ "[" <> T.intercalate ", " (map render exprs) <> "]"
      Array (ArrayRange start stop) -> return $ "[" <> render start <> ".." <> render stop <> "]"
      Ref object index -> return $ render'' object <> "[:" <> render index <> "]"
      Lambda arg expr -> return $ render'' arg <> " => " <> render expr
      Case expr alts -> return $ "case " <> render expr <> " of " <> T.intercalate " | " rPairs
        where rPairs = map (\(p, r) -> render p <> " => " <> render r) alts
      Typed expr typ -> return $ render'' expr <> ": " <> render typ
      If c t f -> do
        if' <- line $ "if " <> render c
        else' <- line "else"
        t' <- render' t
        f' <- render' f
        join [if', t', else', f']
      If' c t -> do
        if' <- line $ "if " <> render c
        t' <- render' t
        join [if', t']
      While ex blk -> do
        while <- line $ "while " <> render ex
        blk' <- render' blk
        join [while, blk']
      For pat expr blk -> do
        for <- line $ "for " <> render pat <> " in " <> render expr
        blk' <- render' blk
        join [for, blk']
      Define name expr -> line $ name <> " = " <> render expr
      Extend name expr -> line $ name <> " &= " <> render expr
      Assign e1 block -> line $ render e1 <> " := " <> render block
      Break e -> line $ "break " <> render e
      Throw e -> line $ "throw " <> render e
      Return e -> line $ "return " <> render e
      TypeDef name typ -> line $ "typedef " <> name <> " = " <> render typ
      _ -> return $ T.pack $ show stmt
    line str = get >>= \i -> return $ T.replicate i " " <> str
    join = return . T.intercalate "\n"
    block blk = up *> fmap (T.intercalate "; ") (mapM render' blk) <* down
    (up, down) = (modify (+2), modify (\n -> n - 2))
    render'' expr = case expr of
      Apply _ _ -> parens
      Dot _ _ -> parens
      Lambda _ _ -> parens
      _ -> render expr
      where parens = "(" <> render expr <> ")"

instance Render Block where
  render b = "{" <> (line . trim . T.intercalate "; " . map render) b <> "}"

instance Render Type where
  render t = case t of
    TRigidVar name -> name
    TPolyVar name -> {-"*" <> -} name {-<> "*"-}
    TConst name -> name
    TTuple ts -> "(" <> T.intercalate ", " (map render ts) <> ")"
    TApply (TConst "[]") typ -> "[" <> render typ <> "]"
    TApply (TConst "[!]") typ -> "[!" <> render typ <> "]"
    TApply a b -> render a <> " " <> render' b
    TFunction t1 t2 -> render'' t1 <> " -> " <> render t2
    TMut typ -> "mut " <> render typ
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
            in T.intercalate ", " (map rPair pairs)

symChars :: String
symChars = "><=+-*/^~!%@&$:.#|?"

isSymbol :: T.Text -> Bool
isSymbol = T.all (`elem` symChars)

binary :: Name -> Expr -> Expr -> Expr
binary name e1 e2 = Apply (Var name) $ Tuple [e1, e2]

boolT, numT, strT, charT, unitT :: Type
boolT = tConst "Bool"
numT = tConst "Num"
strT = tConst "Str"
charT = tConst "Char"
unitT = tTuple []

arrayOf, listOf, setOf, maybeT :: Type -> Type
arrayOf = TApply (TConst "[]")
listOf = TApply (TConst "[!]")
setOf = TApply (TConst "{s}")
maybeT = TApply (TConst "Maybe")
tTuple :: [Type] -> Type
tTuple = TTuple
tConst :: Name -> Type
tConst = TConst
mapOf (key, val) = TApply (TApply (TConst "{}") key) val

(==>) :: Type -> Type -> Type
(==>) = TFunction
infixr 4 ==>
