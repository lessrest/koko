import Test.Hspec
import Koko

main :: IO ()
main = hspec $ do
  describe "parse failures" $ do
    let failsOn = expectParseFailure
        
    failsOn "%0"
    failsOn "a b"
    failsOn `mapM_` (map (:"") "[]{}")

  describe "parse successes" $ do
    let (->>) = shouldParseTo

    "@"         ->> EVar "@"
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

  describe "evaluation" $ do
    let (->>) = shouldEvaluateTo

    "a" ->> VSym "a"
    "[ { a } ]" ->> VSym "a"

  describe "output" $ do
    let (->>) = shouldOutput

    "a" ->> []
    "[ @print a ]" ->> ["a"]
    "[ @print Hello, world! ]" ->> ["Hello,", "world!"]

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

shouldEvaluateTo :: String -> Value -> Spec
shouldEvaluateTo s v =
  it ("should evaluate `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e ->
        case evaluate e of
          (Left err, _) -> expectationFailure (show err)
          (Right v', []) -> v' `shouldBe` v
          (Right _, _) -> expectationFailure "spurious output"

shouldOutput :: String -> [String] -> Spec
shouldOutput s xs =
  it ("should give output for `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e ->
        case evaluate e of
          (Left err, _) -> expectationFailure (show err)
          (Right _, xs') -> xs' `shouldBe` xs
