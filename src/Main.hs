{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
module Main where

import System.Console.Haskeline
import System.Exit
import Control.Monad (forever)
import "mtl" Control.Monad.State (lift)

import Evaluator (EvalState, initState, evalItWith)

repl :: IO ()
repl = do
  putStrLn "Welcome to the Llama REPL."
  runInputT defaultSettings loop'
  where
    loop' = do
      state <- lift initState
      loop state
    loop :: EvalState -> InputT IO ()
    loop eState = forever $ do
      getInputLine "llama> " >>= \case
        Nothing -> do
          lift $ putStrLn "Goodbye"
          lift $ exitSuccess
        Just "" -> return ()
        Just "clear" -> do
          lift $ putStrLn "Cleared variables"
          state <- lift initState
          loop state
        Just "exit" -> do
          lift $ putStrLn "Goodbye"
          lift $ exitSuccess
        Just input -> do
          (_, eState') <- lift $ evalItWith eState input
          loop eState'

main :: IO ()
main = repl
