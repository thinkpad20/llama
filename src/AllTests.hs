module AllTests where

import Common
import qualified DesugarTests as D
import qualified TypeCheckerTests as T
import qualified ParserTests as P
import Tests

main = runAllTests (P.allTests <> T.allTests <> D.allTests)
