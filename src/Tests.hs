{-# LANGUAGE LambdaCase #-}
module Tests (Test(..), runTests) where

import Common hiding (line, addError, Name)
import System.IO
import System.Console.ANSI

import qualified Data.Map as M
type Name = String
data TestResult = TestSuccess [Name]
                | TestFailure [Name] String String String
                | TestError [Name] String String
                | TestExpectedError [Name] String String

data TesterState = TesterState { indentLevel::Int
                               , spaceCount::Int
                               , groupNames::[Name]
                               , successes::[TestResult]
                               , failures::[TestResult]
                               , errors::[TestResult]
                               , expectedErrors::[TestResult]
                               , names::M.Map Name Int}
type Tester = StateT TesterState IO
data Test input result = Test Name input result
                       | SkipTest Name input result
                       | TestGroup Name [Test input result]
                       | SkipTestGroup Name [Test input result]
                       | ShouldError Name input

defaultState = TesterState { indentLevel = 0
                           , spaceCount = 4
                           , successes = []
                           , errors = []
                           , failures = []
                           , groupNames = []
                           , expectedErrors = []
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
runTests' function tests =
  forM_ (zip [1..] tests) $ \(count, test) ->
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
  SkipTest name _ _ ->
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
  ShouldError name input -> do
    withColor Cyan $ putStrI $ show count ++ ". "
    putStr' $ name ++ " (expect to error): "
    -- Run the function on the input; it should fail.
    case function input of
      -- no errors, which isn't what we want
      Right result -> do
        addExpectedError name input result
        withColor Magenta $ putStrLn' "DIDN'T ERROR!"
      -- errors occurred
      Left _ -> withColor Green $ putStrLn' "ERROR AS EXPECTED!"

reportFailure (TestFailure names input expect result) = do
  withColor Magenta $ withUL $ putStrLnI (intercalate ", " names)
  upIndent
  withColor Magenta $ putStrLnI "Input was:"
  withIndent $ withColor Yellow $ putStrLnI input
  withColor Magenta $ putStrLnI "Expected: "
  withIndent $ withColor Yellow $ putStrLnI expect
  withColor Magenta $ putStrLnI "Evaluated to: "
  withIndent $ withColor Yellow $ putStrLnI result
  downIndent

reportError (TestError names input message) = do
  withColor Red $ withUL $ putStrLnI (intercalate ", " names)
  upIndent
  withColor Red $ putStrLnI "Input was:"
  withColor Yellow $ putStrLnI input
  withColor Red $ putStrLnI "Error message: "
  withColor Yellow $ putStrLnI (message ++ "\n")
  downIndent

reportExpectedError (TestExpectedError names input result) = do
  withColor Red $ withUL $ putStrLnI (intercalate ", " names)
  upIndent
  withColor Red $ putStrLnI "Input was:"
  withColor Yellow $ putStrLnI input
  withColor Red $ putStrLnI "Expected an error, but got: "
  withColor Yellow $ putStrLnI (result ++ "\n")
  downIndent

report :: Tester ()
report = do
  s <- length <$> getSuccesses
  fails <- getFailures
  errs <- getErrors
  expErrs <- getExpectedErrors
  let (f, e, ee) = (length fails, length errs, length expErrs)
  let tot = s + f + e + ee
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
  rep ee "didn't throw errors when expected" Cyan
  when (f > 0) $ line >> putStrLn' "Failures:" >> forM_ (reverse fails) reportFailure
  when (e > 0) $ line >> putStrLn' "Errors:" >> forM_ (reverse errs) reportError
  when (ee > 0) $ line >> putStrLn' "Expected Errors:" >> forM_ (reverse expErrs) reportExpectedError
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
getExpectedErrors = get <!> expectedErrors

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
addExpectedError name input result = do
  gNames <- getGroupNames
  let names = reverse $ name : gNames
  let f = TestExpectedError names (show input) (show result)
  modify $ \s -> s { expectedErrors = f : expectedErrors s }
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
