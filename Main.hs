import Koko (parse)
import System.Environment

main :: IO ()
main = getArgs >>= (either (fail . show) print) . parse
