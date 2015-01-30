import Test.Hspec
import qualified Koko as K

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "should parse `@foo'" $ do
      K.parse ["@foo"] `shouldBe` (Just (K.EVar "@foo"))
