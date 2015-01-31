import Koko (evaluate)
import Koko.Parser (parse)
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= (either (fail . show) (run . evaluate)) . parse
  where run (Left v, o) = mapM_ putStr o >> print v >> exitFailure
        run (Right _, o) = mapM_ putStr o
                        
