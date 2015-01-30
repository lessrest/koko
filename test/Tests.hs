import Test.Hspec

import Koko

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    failsOn "%0"
    failsOn "a b"
    
    "@foo"      ->> EVar "@foo"
    "{ }"       ->> EAbs ENil
    "{ a }"     ->> EAbs (ESym "a")
    "{ { a } }" ->> EAbs (EAbs (ESym "a"))
    "[ { a } ]" ->> EApp (EAbs (ESym "a")) []
    "%"         ->> EIdx 1   
    "%1"        ->> EIdx 1
    "%25"       ->> EIdx 25
      
    "[ @print Hello, world! ]" ->>
      EApp (EVar "@print") [ESym "Hello,", ESym "world!"]
    
  where (->>) = shouldParseTo

shouldParseTo :: String -> Expr -> Spec
shouldParseTo s e =
  it ("should parse `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e' -> e' `shouldBe` e

failsOn :: String -> Spec
failsOn s =
  it ("fails on `" ++ s ++ "'") $
    case parse (words s) of
      Left _ -> return ()
      Right x -> expectationFailure ("Parsed to (" ++ show x ++ ")")
