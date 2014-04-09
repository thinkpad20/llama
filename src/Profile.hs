import Evaluator

input :: String
input = "result = ref 0; for i = ref 1; i <= 100000; i := i + 1 do result := result + i; println result"

main :: IO ()
main = do
  putStrLn "Here we go!"
  evalIt input >>= print
  putStrLn "Done!"
