import Koko (parse, evaluate)
import System.Environment

main :: IO ()
main = getArgs >>= (either (fail . show) (run . evaluate)) . parse
  where run (_, o) = mapM_ putStr o
