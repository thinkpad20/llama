module Language.Llama.Types.TypeCheckDefaults where

import qualified Prelude as P
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H
import Language.Llama.Types.TypeCheckLib


typeMap :: TypeMap
typeMap = H.fromList [("Z", "Nat"), ("S", "Nat" ==> "Nat"),
                      ("Less", "Comparison"), 
                      ("More", "Comparison"),
                      ("Equal", "Comparison"),
                      ("Point", fromBT (a ==> point a)),
                      ("Just", fromBT (a ==> maybe a)),
                      ("Nothing", fromBT (maybe a)),
                      ("True", "Bool"), ("False", "Bool"),
                      ("_+_", poly ["a", "b", "c"] plusT),
                      ("_-_", poly ["a", "b", "c"] minusT),
                      ("_*_", poly ["a", "b", "c"] multT),
                      ("-_", poly ["a"] negT),
                      ("_==_", poly ["a"] eqT),
                      ("_<_", poly ["a"] $ hasComp (a ==> a ==> "Bool")),
                      ("compare", poly ["a"] compT)]
a, b, c :: BaseType
(a, b, c) = (TVar "a", TVar "b", TVar "c")

hasComp :: BaseType -> Type
hasComp = Type (oneTrait' "Compare" ["a"])

compBool :: Type
compBool = hasComp (a ==> a ==> "Bool")

eqT, compT, negT, plusT, minusT, multT :: Type
eqT = Type (oneTrait' "Eq" ["a"]) (a ==> a ==> "Bool")
compT = hasComp (a ==> a ==> "Comparison")
negT = Type (oneTrait' "Negate" ["a"]) (a ==> a)
plusT = Type (oneTrait' "Add" ["a", "b", "c"]) (a ==> b ==> c)
minusT = Type (oneTrait' "Subt" ["a", "b", "c"]) (a ==> b ==> c)
multT = Type (oneTrait' "Mult" ["a", "b", "c"]) (a ==> b ==> c)

poly :: [Name] -> Type -> Polytype
poly vars = Polytype (S.fromList vars)

point, maybe, list :: BaseType -> BaseType
point = apply "Point"
maybe = apply "Maybe"
list = apply "List"

addInstances :: [Instance]
addInstances = [ Instance mempty ["Int", "Int", "Int"]
               , Instance mempty ["Nat", "Nat", "Nat"]
               , Instance (oneTrait' "Add" ["a", "a", "a"]) 
                          [point a, point a, point a]]

addDefaults :: [DefaultInstance]
addDefaults = [ DefaultInstance [Fixed "Int", Fixed "Int", Slot "Int"]
              , DefaultInstance [Fixed "Int", Slot "Int", Fixed "Int"]
              , DefaultInstance [Fixed "Int", Fixed "Float", Slot "Float"]
              , DefaultInstance [Fixed "Float", Fixed "Int", Slot "Float"]
              , DefaultInstance [Fixed "Float", Fixed "Float", Slot "Float"]]

eqInstances :: [Instance]
eqInstances = [ Instance mempty ["Int"]
              , Instance mempty ["Nat"]
              , Instance mempty ["Comparison"]
              , Instance (oneTrait' "Eq" ["a"]) [maybe a]]

eqDefaults :: [DefaultInstance]
eqDefaults = [ DefaultInstance [Fixed (TVar "a"), Slot (TVar "a")]
             , DefaultInstance [Slot (TVar "a"), Fixed (TVar "a")]]

intLitInstances :: [Instance]
intLitInstances = [ Instance mempty ["Int"]
                  , Instance mempty ["Nat"]
                  , Instance mempty ["Float"]] 

intLitDefaults :: [DefaultInstance]
intLitDefaults = [DefaultInstance [Slot "Int"]]

strLitInstances :: [Instance]
strLitInstances = [ Instance mempty ["String"]
                  , Instance mempty ["Char"]]

strLitDefaults :: [DefaultInstance]
strLitDefaults = [DefaultInstance [Slot "String"]]

compInstances :: [Instance]
compInstances = [Instance mempty ["Int"]]

traitMap :: TraitMap
traitMap = H.fromList [
    ("Add", (addInstances, addDefaults))
  , ("Mult", (addInstances, addDefaults))
  , ("Subt", (addInstances, addDefaults))
  , ("Div", (addInstances, addDefaults))
  , ("Eq",  (eqInstances, eqDefaults))
  , ("Compare", (compInstances, mempty))
  , ("IntLiteral", (intLitInstances, intLitDefaults))
  , ("StrLiteral", (strLitInstances, strLitDefaults))
  ]
