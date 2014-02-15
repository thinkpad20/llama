{-# LANGUAGE LambdaCase #-}
module Tests (Test(..), runTests) where

import Common
import System.IO
import Data.List (intercalate)
import System.Console.ANSI
import Control.Exception

import qualified Data.Map as M
data TestResult = TestSuccess [Name]
                | TestFailure [Name] String String String
                | TestError [Name] String String

type Name = String
data TesterState = TesterState { indentLevel::Int
                               , spaceCount::Int
                               , groupNames::[Name]
                               , successes::[TestResult]
                               , failures::[TestResult]
                               , errors::[TestResult]
                               , names::M.Map Name Int}
type Tester = StateT TesterState IO
data Test input result = Test Name input result
                       | SkipTest Name input result
                       | TestGroup Name [Test input result]
                       | SkipTestGroup Name [Test input result]

defaultState = TesterState { indentLevel = 0
                           , spaceCount = 4
                           , successes = []
                           , errors = []
                           , failures = []
                           , groupNames = []
                           , names = mempty }

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
  SkipTestGroup name _ -> do
    putStrI "| "
    addGroupName name
    groupName <- getGroupName
    withColor Blue $ withUL $ putStrLn' $ "(Skipped group " ++ groupName ++ ")"
    removeGroupName
    line
  TestGroup name tests -> do
    putStrI "| "
    addGroupName name
    withColor Cyan $ withUL $ putStrLn' $ "Test " ++ name
    upIndent >> runTests' function tests >> downIndent
    removeGroupName
    line
  SkipTest name _ _ -> do
    withColor Blue $ putStrLnI $ "(skipped test " ++ show count ++ " '" ++ name ++ "')"
  Test name input expect -> do
    -- print the header, e.g. "6. test sky is blue: "
    withColor Cyan $ putStrI $ show count ++ ". "
    putStr' $ name ++ ": "
    -- run the function on the input, one of three possibilities
    case function input of
      -- no errors, and result is what's expected
      Right result | result == expect -> do
        addSuccess name
        withColor Green $ putStrLn' "PASSED!"
      -- no errors, but unexpected result (failure)
      Right result -> do
        addFailure name input expect result
        withColor Magenta $ putStrLn' "FAILED!"
      -- errors occurred
      Left message -> do
        addError name input message
        withColor Red $ putStrLn' "ERROR!"

reportFailure (TestFailure names input expect result) = do
  let name = intercalate ", " names
  withColor Magenta $ withUL $ putStrLnI name
  upIndent
  withColor Magenta $ putStrLnI "Input was:"
  withIndent $ withColor Yellow $ putStrLnI input
  withColor Magenta $ putStrLnI "Expected: "
  withIndent $ withColor Yellow $ putStrLnI expect
  withColor Magenta $ putStrLnI "Evaluated to: "
  withIndent $ withColor Yellow $ putStrLnI result
  downIndent

reportError (TestError names input message) = do
  let name = intercalate ", " names
  withColor Red $ withUL $ putStrLnI name
  upIndent
  withColor Red $ putStrLnI "Input was:"
  withColor Yellow $ putStrLnI input
  withColor Red $ putStrLnI "Error message: "
  withColor Yellow $ putStrLnI (message ++ "\n")
  downIndent

report :: Tester ()
report = do
  s <- length <$> getSuccesses
  fails <- getFailures
  errs <- getErrors
  let (f, e) = (length fails, length errs)
  let tot = s + f + e
  let tot'= (fromIntegral tot)::Double
  line
  withColor Cyan $ putStrLn' $ concat $ replicate 10 "=--="
  withColor Cyan $ putStrLn' $ show tot ++ " tests ran total"
  let rep n testType c = do
        let n' = (fromIntegral n)::Double
        let tests = if n == 1 then "1 test " else  show n ++ " tests "
        let percent = " (" ++ show (round $ 100 * n' / tot') ++ "%)"
        withColor c $ putStrLn' $ tests ++ testType ++ percent
  rep s "passed" Green
  rep f "failed" Magenta
  rep e "had errors" Red
  when (f > 0) $ line >> putStrLn' "Failures:" >> forM_ (reverse fails) reportFailure
  when (e > 0) $ line >> putStrLn' "Errors:" >> forM_ (reverse errs) reportError
  withColor Cyan $ putStrLn' $ concat $ replicate 10 "=--="

indent :: String -> Tester String
indent str = do lev <- getILevel
                sp <- getNSpaces
                str ! lines ! map (replicate (lev*sp) ' ' ++) ! intercalate "\n" ! return

withIndent :: Tester a -> Tester a
withIndent x = upIndent >> x >>= \result -> downIndent >> return result

line = putStrLn' ""
putStrLn', putStr', putStrLnI, putStrI :: String -> Tester ()
putStrLn' s = lift (putStrLn s) >> flush
putStr' s = lift (putStr s) >> flush
putStrLnI str = indent str >>= putStrLn'
putStrI str = indent str >>= putStr'
printI :: Show a => a -> Tester ()
printI = putStrLnI . show
getSuccesses, getFailures, getErrors :: Tester [TestResult]
getSuccesses = get <!> successes
getFailures = get <!> failures
getErrors = get <!> errors

getILevel = indentLevel <$> get
getNSpaces = spaceCount <$> get
addSuccess name = do
  gNames <- getGroupNames
  let names = reverse $ name : gNames
  modify $ \s -> s {successes = TestSuccess [name] : successes s}
addFailure name input expect result = do
  gNames <- getGroupNames
  let names = reverse $ name : gNames
  let f = TestFailure names (show input) (show expect) (show result)
  modify $ \s -> s { failures = f : failures s }
addError name input message = do
  gNames <- getGroupNames
  let names = reverse $ name : gNames
  let e = TestError names (show input) (show message)
  modify $ \s -> s { errors = e : errors s }
getGroupNames = get <!> groupNames
getGroupName = get <!> groupNames <!> reverse <!> intercalate ", "
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
    Test "division" (10, 2) 5 -- passes
  , Test "division" (5, 5) 1 -- passes
  , Test "division" (5, 0) 0 -- error
  , Test "division" (14, 7) 2 -- wrong
  ]

stupidDiv :: (Int, Int) -> Either String Int
stupidDiv (_, 0) = Left "can't divide by zero!"
stupidDiv (14, 7) = Right 3 -- oh noes
stupidDiv (a, b) = Right (a `div` b)

main = runTests stupidDiv [sampleTests]
