{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
module Main where

import System.Console.Haskeline
import System.Exit
import System.Environment
import Control.Monad (forever)
import Data.Monoid
import "mtl" Control.Monad.State (lift)

import Evaluator (EvalState, initState, evalItWith)
import CompileJS (compileIt, jsPreamble)

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

parseArgs :: [String] -> IO ()
parseArgs args = do
  let input = getInput args
      output = case getOutput args of
        Nothing -> input <> ".js"
        Just o -> o
  compileFile input output

getOutput :: [String] -> Maybe FilePath
getOutput [] = Nothing
getOutput ("-o":output:_) = Just output
getOutput (_:rest) = getOutput rest

getInput :: [String] -> FilePath
getInput [] = error "No input file"
getInput ("-c":input:_) = input
getInput [input] = input
getInput (_:rest) = getInput rest

compileFile :: FilePath -> FilePath -> IO ()
compileFile input output = readFile input >>= \i -> case compileIt i of
  Left err -> print err >> exitFailure
  Right js -> writeFile output (jsPreamble <> show js <> "\n")

main :: IO ()
main = getArgs >>= \case
  [] -> repl
  args -> parseArgs args
