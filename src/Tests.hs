module Tests where

import Parser
import Control.Monad

tests = [ ("1", expr $ Number 1)
        , ("foo", expr $ Id "foo")
        , ("1 + 2", expr $ Binary "+" (Number 1, Number 2))]
  where expr e = [Expr e]

runTests = forM_ tests $ \(input, expect) ->
  let result = grab input in
  if result /= expect
  then putStrLn $ "Failed on " ++ show input ++ ": got " ++ show result
  else putStrLn $ "Passed on " ++ show input
