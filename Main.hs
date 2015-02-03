import Koko.Evaluator (evaluateIO)
import Koko.Parser (parse)

import Control.Applicative
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= (either (fail . show) ((run =<<) . evaluateIO)) . parse
  where run (Left v) = print v >> exitFailure
        run (Right _) = return ()
                        
tryIt :: String -> IO ()
tryIt = (either (fail . show) ((run =<<) . evaluateIO)) . parse . words
  where run (Left v) = print v
        run (Right _) = return ()
