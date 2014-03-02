import Common
import Evaluator
import EvaluatorLib
import TypeChecker
import qualified Data.Text as T

input = "i = mut 0; r = mut 0; while i < 100000 {r := r + i; i := i + 1}; r"

main = evalIt defaultState defaultTypingState input >> return ()
