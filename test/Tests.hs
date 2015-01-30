import Test.Hspec

import Koko (parse, Token, Expr,
             Expr (EApp,
                   EVar,
                   ESym))

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    let (->>) = shouldParseTo
    
    it "should parse `@foo'" $ do
      ["@foo"] ->> EApp (EVar "@foo") []
      
    it "should parse `@print Hello, world!'" $ do
      ["@print", "Hello,", "world!"] ->>
        EApp (EVar "@print") [ESym "Hello,", ESym "world!"]

shouldParseTo :: [Token] -> Expr -> Expectation
shouldParseTo xs e =
  case parse xs of
   Left err -> expectationFailure (show err)
   Right e' -> e' `shouldBe` e
