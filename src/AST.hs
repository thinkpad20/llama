{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module AST where

import Prelude (IO, Eq(..), Ord(..), Bool(..),
                Double, String, Maybe(..), Int, Monad(..),
                ($), (.), floor, map, Functor(..), mapM,
                (+), (-), elem, Either(..))
import qualified Prelude as P
import Data.Text hiding (map)
import Data.Monoid

import Common hiding (intercalate)
import TypeLib

type Kwargs = [(Name, Either Expr Type)]
data Expr = Var         !Name
          | Number      !Double
          | String      !Text
          | Constructor !Name
          | Block       !Block
          | Dot         !Expr !Expr
          | Apply       !Expr !Expr
          | Lambda      !Expr !Expr
          | Lambdas     ![(Expr, Expr)]
          | Case        !Expr ![(Expr, Expr)]
          | Tuple       ![Expr] Kwargs
          | Literal     !Literal
          | DeRef       !Expr !Expr
          | Typed       !Expr !Type
          | If          !Expr !Expr !Expr
          | If'         !Expr !Expr
          | While       !Expr !Expr
          | For         !Expr !Expr !Expr !Expr
          | ForIn       !Expr !Expr !Expr
          | Define      !Name !Expr
          | Extend      !Name !Expr
          | Assign      !Expr !Expr
          | Return      !Expr
          | Throw       !Expr
          | Break       !Expr
          | After       !Expr !Expr
          | Before      !Expr !Expr
          | ObjDec      !ObjectDec
          | Modified    !Mod !Expr
          | TypeDef     !Name !Type
          | Prefix      !Name !Expr
          | LambdaDot   !Expr
          | Continue
          | WildCard
          deriving (P.Show, Eq)

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

instance Render Expr where
  render e = evalState (render' e) 0 where
    render' :: Expr -> State Int Text
    render' expr = case expr of
      Var name -> return name
      Constructor name -> return name
      Number n | isInt n -> return $ show $ (floor n :: Int)
      Number n -> return $ show n
      Block blk -> block blk
      String s -> return $ show s
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
      While ex blk -> do
        while <- mkLine $ "while " <> render ex
        blk' <- render' blk
        join [while, blk']
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
      _ -> return $ show expr
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
isSymbol = all (`elem` symChars)

binary :: Name -> Expr -> Expr -> Expr
binary name e1 e2 = Apply (Var name) $ Tuple [e1, e2] mempty

tuple :: [Expr] -> Expr
tuple exprs = Tuple exprs mempty
unit :: Expr
unit = tuple []
