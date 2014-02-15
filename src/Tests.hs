{-# LANGUAGE LambdaCase #-}
module Tests (Test(..), runTests) where

import System.IO
import Data.IORef
import Data.List (intercalate)
import System.Console.ANSI
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Monad.State

type Name = String
data TesterState = TesterState { indentLevel::Int
                               , spaceCount::Int
                               , groupNames::[Name]
                               , successCount::Int
                               , failCount::Int
                               , errorCount::Int }
type Tester = StateT TesterState IO
data Test input result = Test Name input result
                       | TestGroup Name [Test input result]

defaultState = TesterState { indentLevel = 0
                           , spaceCount = 2
                           , successCount = 0
                           , errorCount = 0
                           , failCount = 0
                           , groupNames = [] }

runTests :: (Eq result, Show input, Show result, Show error) =>
            (input -> Either error result) ->
            [Test input result] ->
            IO ()
runTests f ts = do runStateT (runTests' f ts >> report) defaultState
                   return ()

runTests' :: (Eq result, Show input, Show result, Show error) =>
            (input -> Either error result) ->
            [Test input result] ->
            Tester ()
runTests' function tests = do
  forM_ (zip [1..] tests) $ \(count, test) -> do
    runTest function count test

runTest :: (Eq result, Show input, Show result, Show error) =>
           (input -> Either error result) ->
           Int ->
           Test input result ->
           Tester ()
runTest function count test = case test of
  TestGroup name tests -> do
    putStrI ""
    addGroupName name
    groupName <- getGroupName
    withColor Cyan $ withUL $ putStrLnI $ "Test Group " ++ groupName
    upIndent >> runTests' function tests >> downIndent
    removeGroupName
    putStrI ""
  Test name input expect -> do
    -- print the header, e.g. "Test 6 (test sky is blue): "
    withColor Blue $ putStrI $ "Test " ++ show count
    putStr' $ " (" ++ name ++ "): "
    -- run the function on the input, one of three possibilities
    case function input of
      -- no errors, and result is what's expected
      Right result | result == expect -> do
        addSuccess
        withColor Green $ putStrLn' "PASSED!"
      -- no errors, but unexpected result
      Right result -> do
        addFailure
        withColor Magenta $ putStrLn' "FAILED!" >> putStrLnI "Input was:"
        withIndent $ withColor Yellow $ printI input
        withColor Magenta $ putStrLnI "Expected: "
        withIndent $ withColor Yellow $ printI expect
        withColor Magenta $ putStrLnI "Evaluated to: "
        withColor Yellow $ printI result
      -- errors occurred
      Left err -> do
        addError
        withColor Red $ putStrLn' "ERROR!" >> putStrLnI "Input was:"
        withColor Yellow $ printI input
        withColor Red $ putStrLnI "Error message: "
        withColor Yellow $ putStrLnI (show err ++ "\n")

(!) = flip ($)
(<!>) = flip (<$>)

report = do
  s <- get <!> successCount
  f <- get <!> failCount
  e <- get <!> errorCount
  let tot = s + f + e
  putStrLn' $ "Total tests: " ++ show tot
  let tot'= (fromIntegral tot)::Double
  let rep n t c = do
        let n' = (fromIntegral n)::Double
        withColor c $ putStrLn' $
                       concat $ [show n, " tests ", t, " (",
                                 show (100 * n' / tot'), "%)"]
  rep s "passed" Green
  rep f "failed" Magenta
  rep e "had errors" Red

indent :: String -> Tester String
indent str = do lev <- getILevel
                sp <- getNSpaces
                str ! lines ! map (replicate (lev*sp) ' ' ++) ! intercalate "\n" ! return

withIndent :: Tester a -> Tester a
withIndent x = upIndent >> x >>= \result -> downIndent >> return result

putStrLn', putStr', putStrLnI, putStrI :: String -> Tester ()
putStrLn' s = lift (putStrLn s) >> flush
putStr' s = lift (putStr s) >> flush
putStrLnI str = indent str >>= putStrLn'
putStrI str = indent str >>= putStr'
printI :: Show a => a -> Tester ()
printI = putStrLnI . show

getILevel = indentLevel <$> get
getNSpaces = spaceCount <$> get
addSuccess = modify $ \s -> s { successCount = successCount s + 1 }
addFailure = modify $ \s -> s { failCount = failCount s + 1 }
addError = modify $ \s -> s { errorCount = errorCount s + 1 }
getGroupName = get <!> groupNames <!> reverse <!> intercalate " -> "
addGroupName name = modify (\s -> s { groupNames = name : groupNames s } )
removeGroupName = modify (\s -> s {groupNames = tail $ groupNames s})
upIndent :: Tester ()
upIndent = modify (\s -> s {indentLevel = indentLevel s + 1})
downIndent :: Tester ()
downIndent = modify (\s -> s {indentLevel = indentLevel s - 1})

flush :: Tester ()
flush = lift $ hFlush stdout

withColor :: Color -> Tester a -> Tester a
withColor color action = do
  lift $ setSGR [SetColor Foreground Vivid color]
  result <- action
  lift $ setSGR [Reset]
  return result

withUL :: Tester a -> Tester a
withUL action = do
  lift $ setSGR [SetUnderlining SingleUnderline]
  result <- action
  lift $ setSGR [SetUnderlining NoUnderline]
  return result

sampleTests = TestGroup "Division"
  [
    Test "division 1" (10, 2) 5 -- passes
  , Test "division 2" (5, 5) 1 -- passes
  , Test "division 3" (5, 0) 0 -- error
  , Test "division 4" (14, 7) 2 -- wrong
  ]

stupidDiv :: (Int, Int) -> Either String Int
stupidDiv (_, 0) = Left "can't divide by zero!"
stupidDiv (14, 7) = Right 3 -- oh noes
stupidDiv (a, b) = Right (a `div` b)

main = runTests stupidDiv [sampleTests]
