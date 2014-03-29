{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module PurityChecker where

import qualified Data.Map as M
import System.IO.Unsafe
import Data.Either (lefts)

import Common
import AST
import Parser
import TypeLib

recordPurity :: Name -> PurityType -> Typing ()
recordPurity name p = do
  nsName <- fullName name
  modify $ \s -> s { purityEnv = M.insert nsName p (purityEnv s)}

ptLookup :: Name -> Typing (Maybe PurityType)
ptLookup name = do
  ns <- get <!> nameSpace
  loop ns
  where look name = M.lookup name . purityEnv <$> get
        loop [] = look name
        loop (n:ns) = look (render $ name:n:ns) >>= \case
          Just typ -> return (Just typ)
          Nothing -> loop ns

class GetPurity a where
  getPurity :: a -> Typing PurityType

instance GetPurity Expr where
  getPurity e = case e of
    Number _ -> return PTPure
    String _ -> return PTPure
    Literal _ -> return PTPure
    Var name -> ptLookup name >>= \case
      Nothing -> throwError1 (name <> " is not defined")
      Just ptype -> return ptype
    Lambda arg expr -> do
      pushNameSpace "%l"
      purity <- mappend <$> getPurity arg <*> getPurity expr
      popNameSpace
      return purity
    Define name expr -> do
      pushNameSpace name
      purity <- getPurity expr
      popNameSpace
      recordPurity name purity
      return purity
    Tuple exprs kw -> do
      let kwExprs = map snd kw ! lefts
      mconcatMapM getPurity (exprs <> kwExprs)
    Apply e1 e2 -> mappend <$> getPurity e1 <*> getPurity e2
    Dot e1 e2 -> getPurity (Apply e2 e1)

instance GetPurity Block where
  getPurity block = mconcatMapM getPurity block

runPurityType :: GetPurity a => a -> (Either ErrorList PurityType, TypingState)
runPurityType x =
  unsafePerformIO $ runStateT (runErrorT $ getPurity x) defaultTypingState

testPurityType :: String -> Either ErrorList PurityType
testPurityType input = case grab input of
  Left err -> error $ show err
  Right block -> fst $ runPurityType block
