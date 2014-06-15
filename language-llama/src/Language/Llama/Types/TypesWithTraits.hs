{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
module Language.Llama.Types.TypesWithTraits where
import qualified Prelude as P
import Prelude (Show)
import Data.Set (Set, singleton, (\\))
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Llama.Common.Common hiding (singleton)
data BaseType
  = TVar Name
  | TConst Name
  | TApply BaseType BaseType
  deriving (Show, Eq, Ord)
type Map = HashMap
data Trait = Trait Name [BaseType] deriving (Show, Eq, Ord)
newtype Context = Context (Set Trait) deriving (Show, Eq)
data Type = Type Context BaseType deriving (Show, Eq)
data Polytype = Polytype (Set Name) Type deriving (Show, Eq)
newtype Substitution = Substitution (Map Name BaseType)
type TypeMap = Map Name Polytype
type TraitMap = Map Name (Set [BaseType])
newtype TypeEnv = TypeEnv [(TypeMap, TraitMap)] deriving (Show, Eq)

type TCState = ()
type TypeChecker = ErrorT ErrorList (State TCState)

instance Monoid Context where
  mempty = Context mempty
  mappend (Context c1) (Context c2) = Context (mappend c1 c2)

instance Monoid Substitution where
  mempty = Substitution mempty
  mappend s@(Substitution s1) (Substitution s2) =
    Substitution (fmap (substitute s) s2 <> s1)

instance IsString BaseType where fromString = TConst . pack
instance IsString Type where fromString = Type mempty . fromString
instance IsString Polytype where fromString = Polytype mempty . fromString

instance Render BaseType where
  render t = case t of
    TVar name -> name
    TApply (TApply "->" t1) t2 -> render'' t1 <> " -> " <> render t2
    TConst name -> name
    TApply t1 t2 -> render' t1 <> " " <> render' t2
    where render' t@(TApply _ _ ) = "(" <> render t <> ")"
          render' t = render t
          render'' t@(TApply (TApply "->" t1) t2) = "(" <> render t <> ")"
          render'' t = render t

class IsString a => Apply a where apply :: a -> a -> a
instance Apply BaseType where
  apply = TApply
instance Apply Type where
  apply (Type ctx1 t1) (Type ctx2 t2) = Type (ctx1 <> ctx2) (apply t1 t2)
instance Apply Polytype where
  apply (Polytype vs1 t1) (Polytype vs2 t2) =
    Polytype (vs1 <> vs2) (apply t1 t2)

class FreeVars a where freevars :: a -> Set Name
instance FreeVars BaseType where
  freevars = \case
    TVar n -> singleton n
    TConst _ -> mempty
    TApply t1 t2 -> freevars t1 <> freevars t2
instance FreeVars Trait where
  freevars (Trait _ types) = freevars types
instance FreeVars Context where
  freevars (Context ctx) = freevars $ toList ctx
instance FreeVars Type where
  freevars (Type ctx t) = freevars ctx <> freevars t
instance FreeVars Polytype where
  freevars (Polytype vars t) = freevars t \\ vars
instance FreeVars a => FreeVars [a] where
  freevars = mconcat . fmap freevars

class Substitutable a where substitute :: Substitution -> a -> a
instance Substitutable BaseType where
  substitute subs@(Substitution s) t = case t of
    TVar n | member n s -> s H.! n
    TApply t1 t2 -> TApply (substitute subs t1) (substitute subs t2)
    _ -> t
instance Substitutable Trait where
  substitute subs (Trait n ts) = Trait n $ fmap (substitute subs) ts
instance Substitutable Context where
  substitute subs (Context ctx) = Context $ S.map (substitute subs) ctx
instance Substitutable Type where
  substitute subs (Type ctx t) = Type (s ctx) (s t) where s = substitute subs
instance Substitutable Polytype where
  substitute subs (Polytype vars t) = (Polytype vars (s t)) where
    s = substitute (S.foldr' remove subs vars)

teLookup :: Name -> TypeEnv -> Maybe Polytype
teLookup name (TypeEnv env) = go (fmap fst env) where
  go [] = Nothing
  go (e:rest) = case lookup name e of
    Nothing -> go rest
    Just pt -> return pt

lookup1 :: Name -> TypeEnv -> Maybe Polytype
lookup1 name (TypeEnv []) = Nothing
lookup1 name (TypeEnv ((e, _):_)) = H.lookup name e

generalize :: Type -> Polytype
generalize (Type ctx t) = do
  let freeFromT = freevars t
      freeFromCtx = freevars ctx
  Polytype (freeFromT \\ freeFromCtx) (Type ctx t)

-- | Takes and returns an integer to denote where to begin renaming from.
instantiate :: Int -> Polytype -> (Type, Int)
instantiate n (Polytype vars t) = go nsVars t where
  nsVars = P.zip [n..] (toList vars)
  sub var num = Substitution $ H.singleton var (TVar $ "t" <> render num)
  go [] _ = (t, n)
  go [(num, var)] type_ = (substitute (sub var num) type_, num + 1)
  go ((num, var):rest) type_ = do
    go rest (substitute (sub var num) type_)

remove :: Name -> Substitution -> Substitution
remove n (Substitution s) = Substitution (delete n s)

(==>) :: Apply a => a -> a -> a
t1 ==> t2 = apply (apply "->" t1) t2

tuple :: Apply a => [a] -> a
tuple ts = do
  let name = "Tuple(" <> P.show (length ts) <> ")"
  foldl' apply (fromString name) ts

unify :: BaseType -> BaseType -> TypeChecker Substitution
unify t1 t2 = case (t1, t2) of
  (TVar a, _) -> return (Substitution (H.singleton a t2))
  (_, TVar a) -> return (Substitution (H.singleton a t1))
  (TConst n1, TConst n2) | n1 == n2 -> return mempty
  (TApply a1 a2, TApply b1 b2) -> do
    s1 <- unify a1 b1
    s2 <- unify (substitute s1 a2) (substitute s1 b2)
    return (s1 <> s2)
  (_, _) -> throwErrorC ["Can't unify ", render t1, " and ", render t2]

checkContext :: TypeEnv -> Context -> TypeChecker ()
checkContext (TypeEnv env) (Context ctx) = mapM_ check ctx where
  check :: Trait -> TypeChecker ()
  check (Trait name ts) =
    -- If there are any free variables, we assume it's valid
    if not (S.null $ freevars ts) then return ()
    -- Otherwise, make sure it's in the TraitMap
    else search (fmap snd env)
    where
      search [] = throwError1 $ "No instance of " <> whatsMissing
      search (tmap:rest) = case lookup name tmap of
        Just set | S.member ts set -> return ()
        Nothing -> search rest
      whatsMissing = case ts of
        [] -> name
        [t] -> name <> " for type " <> render t
        ts -> name <> " for types " <> commaSep ts
      commaSep = T.intercalate ", " . map render
