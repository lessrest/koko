import Test.Hspec
import Koko

main :: IO ()
main = hspec $ do
  let (->>) = shouldParseTo
      (=>>) = shouldEvaluateTo
      (=*>) = shouldOutput
  
  describe "parse failures" $ do
    expectParseFailure "%0"
    expectParseFailure "a b"
    expectParseFailure `mapM_` (map (:"") "[]{}")
    expectParseFailure "[ ]"

  describe "parse successes" $ do
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
    "a"         =>> VSym "a"
    "[ { a } ]" =>> VSym "a"

  describe "output" $ do
    "a"                             =*> []
    "[ @print-line a ]"             =*> ["a\n"]
    "[ @print-line Hello, world! ]" =*> ["Hello, world!\n"]
    "[ [ { @print-line } ] yes ]"   =*> ["yes\n"]

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
