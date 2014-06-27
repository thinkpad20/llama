{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Language.Llama.Common.Types (
    CanApply(..), FromBaseType(..), Normalize(..), FreeVars(..), Type(..)
  , BaseType(..), Polytype(..), Context(..), Assertion(..)
  , isConstant, freelist, normalize, tuple, (==>)
  ) where

import qualified Prelude as P
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Set (Set, singleton, (\\))
import qualified Data.Set as S

import Language.Llama.Common.Common

data BaseType
  = TVar Name
  | TConst Name
  | TApply BaseType BaseType
  deriving (P.Show, Eq, Ord)
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

class CanApply a where apply :: a -> a -> a
instance CanApply BaseType where
  apply = TApply
instance CanApply Type where
  apply (Type ctx1 t1) (Type ctx2 t2) = Type (ctx1 <> ctx2) (apply t1 t2)
instance CanApply Polytype where
  apply (Polytype vs1 t1) (Polytype vs2 t2) =
    Polytype (vs1 <> vs2) (apply t1 t2)

-- | The function type, which is actually a rank-2 type applied twice.
(==>) :: (IsString a, CanApply a) => a -> a -> a
t1 ==> t2 = apply (apply "->" t1) t2

infixr 3 ==>

-- | A tuple type, a rank-N type where N is the length of the tuple.
tuple :: (IsString a, CanApply a) => [a] -> a
tuple ts = do
  let name = "Tuple(" <> P.show (length ts) <> ")"
  foldl' apply (fromString name) ts

class FromBaseType t where
  fromBT :: BaseType -> t

instance FromBaseType BaseType where 
  fromBT bt = bt
instance FromBaseType Type where 
  fromBT = Type mempty
instance FromBaseType Polytype where 
  fromBT bt = Polytype (freevars bt) (fromBT bt)

-- | Class of things which contain free variables. @freevars@ gets all of the
--  free variables out of a type. For example the type @a@ has free variables 
-- {a}; the type @a -> b@ has free variables {a, b}; the type @Maybe (a -> 
-- Int) -> b -> b@ has free variables {a, b}, etc.
class FreeVars a where freevars :: a -> Set Name
instance FreeVars BaseType where
  freevars = \case
    TVar n -> singleton n
    TConst _ -> mempty
    TApply t1 t2 -> freevars t1 <> freevars t2
instance FreeVars Assertion where
  freevars (HasTrait _ types) = freevars types
instance FreeVars Context where
  freevars (Context ctx) = freevars $ toList ctx
instance FreeVars Type where
  freevars (Type ctx t) = freevars ctx <> freevars t
instance FreeVars Polytype where
  freevars (Polytype vars t) = freevars t \\ vars
instance FreeVars a => FreeVars [a] where
  freevars = mconcat . fmap freevars

isConstant :: FreeVars a => a -> Bool
isConstant = S.null . freevars

freelist :: FreeVars a => a -> [Name]
freelist = S.toList . freevars

-- | Normalizing means replacing obscurely-named type variables with letters.
-- For example, the type @(t$13 -> [t$4]) -> t$13@ would be @(a -> b) -> a@.
-- The best way to do this is with a state monad so that we can track which
-- renamings have been done. So the only method that we need is @normalizeS@ 
-- (@S@ for state monad). This lets us normalize across multiple types.
class Normalize t where
  normalizeS :: t -> State (Text, HashMap Name Name) t

normalize :: Normalize a => a -> a
normalize = normalizeWith ("a", mempty)

normalizeWith :: Normalize a => (Text, Map Name Name) -> a -> a
normalizeWith state x = evalState (normalizeS x) state

instance Normalize BaseType where
  normalizeS type_ = case type_ of
    TVar name -> do
      (name', mapping) <- get
      case H.lookup name mapping of
        Nothing -> do modify (\(n, m) -> (next n, H.insert name name' m))
                      return (TVar name')
        Just n -> return (TVar n)
    TApply a b -> TApply <$> normalizeS a <*> normalizeS b
    _ -> return type_
    where
      next name = case T.last name of
        c | c < 'z' -> T.init name `T.snoc` succ c
          | True    -> name `T.snoc` 'a'

instance Normalize Type where
  normalizeS (Type ctx t) = Type <$> normalizeS ctx <*> normalizeS t

instance Normalize Context where
  normalizeS (Context ctx) = do
    let asserts = S.toList ctx
    asserts' <- forM asserts $ \case
      HasTrait n ts -> HasTrait n <$> normalizeS ts
    -- Filter out constant assertions (e.g. {Foo Int}) unless showConstants
    return $ Context $ S.fromList $
      if showConstants then asserts' else filter (not . isConstant) asserts'

instance Normalize a => Normalize [a] where
  normalizeS = mapM normalizeS

showConstants = False
