import Test.Hspec

import Koko

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    let (->>) = shouldParseTo
    
    it "should parse `@foo'" $ do
      ["@foo"] ->> EVar "@foo"
      
    it "should parse `[ @print Hello, world! ]'" $ do
      words "[ @print Hello, world! ]" ->>
        EApp (EVar "@print") [ESym "Hello,", ESym "world!"]

    it "should parse `{ }'" $ do
      words "{ }" ->> EAbs ENil

    it "should parse `{ a }'" $ do
      words "{ a }" ->> EAbs (ESym "a")

    it "should parse `{ { a } }'" $ do
      words "{ { a } }" ->> EAbs (EAbs (ESym "a"))

shouldParseTo :: [Token] -> Expr -> Expectation
shouldParseTo xs e =
  case parse xs of
   Left err -> expectationFailure (show err)
   Right e' -> e' `shouldBe` e
