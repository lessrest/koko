import Test.Hspec
import Koko

main :: IO ()
main = hspec $ do
  describe "parse failures" $ do
    let failsOn = expectParseFailure
        
    failsOn "%0"
    failsOn "a b"
    failsOn `mapM_` (map (:"") "@[]{}")

  describe "parse successes" $ do
    let (->>) = shouldParseTo
        
    "@foo"      ->> EVar "@foo"
    "{ }"       ->> EAbs ENil
    "{ a }"     ->> EAbs (ESym "a")
    "{ { a } }" ->> EAbs (EAbs (ESym "a"))
    "[ { a } ]" ->> EApp (EAbs (ESym "a")) []
    "%"         ->> EIdx 1   
    "%1"        ->> EIdx 1
    "%25"       ->> EIdx 25

  describe "larger examples" $ do
    "[ @print Hello, world! ]" ->>
      EApp (EVar "@print") [ESym "Hello,", ESym "world!"]

------------------------------------------------------------------------

shouldParseTo :: String -> Expr -> Spec
shouldParseTo s e =
  it ("should parse `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e' -> e' `shouldBe` e

expectParseFailure :: String -> Spec
expectParseFailure s =
  it ("fails on `" ++ s ++ "'") $
    case parse (words s) of
      Left _ -> return ()
      Right x -> expectationFailure ("Parsed to (" ++ show x ++ ")")
