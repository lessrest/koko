import Test.Hspec

import Koko

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    let (->>) = shouldParseTo
    
    it "should parse `@foo'" $ do
      ["@foo"] ->> EVar "@foo"
      
    it "should parse `[ @print Hello, world! ]'" $ do
      ["[", "@print", "Hello,", "world!", "]"] ->>
        EApp (EVar "@print") [ESym "Hello,", ESym "world!"]

    it "should parse `{ }'" $ do
      ["{", "}"] ->> EAbs ENil

shouldParseTo :: [Token] -> Expr -> Expectation
shouldParseTo xs e =
  case parse xs of
   Left err -> expectationFailure (show err)
   Right e' -> e' `shouldBe` e
