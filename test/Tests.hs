import Test.Hspec

import qualified Koko as K

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    let (->>) = shouldParseTo
    
    it "should parse `@foo'" $ do
      ["@foo"] ->> (K.EApp (K.EVar "@foo") [])
      
    it "should parse `@print Hello, world!'" $ do
      ["@print", "Hello,", "world!"] ->>
        (K.EApp (K.EVar "@print") [K.ESym "Hello,", K.ESym "world!"])

shouldParseTo :: [K.Token] -> K.Expr -> Expectation
shouldParseTo xs e =
  case K.parse xs of
   Left err -> expectationFailure (show err)
   Right e' -> e' `shouldBe` e
