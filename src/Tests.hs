{-# LANGUAGE LambdaCase #-}
module Tests where

import System.IO
import Data.IORef
import System.Console.ANSI
import Control.Monad
import Control.Exception

newtype Test input result = Test (String, input, result)
newtype TestGroup input result = TestGroup (String, [Test input result])

runTests :: (Eq result, Show input, Show result, Show error) =>
            (input -> Either error result) -> 
            [TestGroup input result] -> 
            IO ()
runTests function testGroups = do
  groupCounter <- newIORef 1
  forM_ testGroups $ \(TestGroup (name, tests)) -> do
    group <- readIORef groupCounter
    withColor Cyan $ withUL $ putStrLn $ 
      "Test Group " ++ show group ++ ": " ++ name
    counter <- newIORef 1
    forM_ tests $ \(Test (name, input, expect)) -> do
      -- print the header, e.g. "1 (test sky is blue): "
      count <- readIORef counter
      withColor Blue $ putStr $ "Test " ++ show count
      putStr (" (" ++ name ++ "): ") >> hFlush stdout
      -- run the function on the input, one of three possibilities
      case function input of
        -- no errors, and result is what's expected
        Right result | result == expect ->
          withColor Green $ putStrLn "PASSED!"
        -- no errors, but unexpected result
        Right result -> do
          withColor Red $ putStrLn "FAILED!\nInput was:"
          withColor Yellow $ print input
          withColor Red $ putStrLn "Expected: "
          withColor Yellow $ print expect
          withColor Red $ putStrLn "Evaluated to: "
          withColor Yellow $ print result
        -- errors occurred
        Left err -> do
          withColor Red $ putStrLn "ERROR!\nInput was:"
          withColor Yellow $ print input
          withColor Red $ putStrLn "Error message: "
          withColor Yellow $ putStrLn (show err ++ "\n")
      modifyIORef counter (+1)
    putStrLn ""
    modifyIORef groupCounter (+1)


withColor :: Color -> IO a -> IO a
withColor color action = do
  setSGR [SetColor Foreground Vivid color]
  result <- action
  setSGR [Reset]
  return result

withUL :: IO a -> IO a
withUL action = do
  setSGR [SetUnderlining SingleUnderline] 
  result <- action
  setSGR [SetUnderlining NoUnderline]
  return result
