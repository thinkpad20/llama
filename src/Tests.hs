module Tests where

import Parser
import System.IO
import System.Console.ANSI
import Control.Monad
import Control.Exception

newtype Test input result = Test (String, input, result)

tests = [ Test ("number", "1", expr $ Number 1)
        , Test ("variable", "foo", expr $ Id "foo")
        , Test ("binary", "1 + 2", expr $ Binary "+" (Number 1, Number 2))
        , Test ("observes precedence rules 1"
               , "1 + 2 * 3"
               , expr $ (Binary "+" (Number 1, Binary "*" (Number 2, Number 3))))
        , Test ("observes precedence rules 2"
               , "1 * 2 + 3"
               , expr $ (Binary "+" (Binary "*" (Number 1, Number 2), Number 3)))
        , Test ("observes associativity rules 1"
               , "1 - 2 - 3"
               , expr $ (Binary "-" (Binary "-" (Number 1, Number 2), Number 3)))
        , Test ("apply", "foo bar", expr $ Apply (Id "foo") (Id "bar"))
        , Test ("while block without whitespace", "while 1 {2}"
               , single $ While (Number 1) (expr $ Number 2))
        , Test ("while block without whitespace, multiple statements", "while 1 {2; 3}"
               , single $ While (Number 1) [Expr $ Number 2, Expr $ Number 3])
        , Test ("while block with whitespace", "while 1\n  2"
               , single $ While (Number 1) (expr $ Number 2))
        , Test ("while block with whitespace, multiple statements", "while 1\n  2\n  3"
               , single $ While (Number 1) [Expr $ Number 2, Expr $ Number 3])
        , Test ("while block with whitespace, trailing newline", "while 1\n  2\n"
               , single $ While (Number 1) (expr $ Number 2))
        , Test ("while block with binary condition", "while 1 < 2\n  print 3\n"
               , single $ While (Binary "<" (Number 1, Number 2))
                            (expr $ Apply (Id "print") (Number 3)))
        ]

  where expr e = [Expr e]

runTests = forM_ tests $ \(Test (name, input, expect)) -> do
  let input' = if '\n' `elem` input then '\n':input else input
  putStr (name ++ " ") >> hFlush stdout
  result <- grab input
  if result /= expect
  then do
    withColor Red $ putStrLn $ "FAILED: input was "
    withColor Yellow $ putStrLn $ show input
    withColor Red $ putStrLn $ "Expected: "
    withColor Yellow $ putStrLn $ show expect
    withColor Red $ putStrLn $ "Evaluated to: "
    withColor Yellow $ putStrLn $ show result
  else do
    withColor Green $ putStrLn "PASSED!"

withColor :: Color -> IO a -> IO a
withColor color action = do
  setSGR [SetColor Foreground Vivid color]
  result <- action
  setSGR [Reset]
  return result

main = runTests
