import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "test suite" $ do
    it "should work" (True `shouldBe` True)
