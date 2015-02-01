{-# LANGUAGE GADTs #-}
module Main where

import Test.Hspec

import Control.Monad

import Koko.Evaluator
import Koko.Types
import Koko.Parser

main :: IO ()
main = hspec $ do
  let (->>) = shouldParseTo
      (=>>) = shouldEvaluateTo
      (===) = shouldEvaluateEquivalently
      (=*>) = shouldOutput
  
  describe "parse failures" $ do
    expectParseFailure "%0"
    expectParseFailure "a b"
    expectParseFailure `mapM_` (map (:"") "[]{}")
    expectParseFailure "[ ]"

  describe "parse successes" $ do
    "@"         ->> eVar noAnn (Right "@")
    "@foo"      ->> eVar noAnn (Right "@foo")
    "{ }"       ->> absN noAnn (eNil noAnn)
    "{ a }"     ->> absN noAnn (eSym noAnn "a")
    "{ { a } }" ->> absN noAnn (absN noAnn (eSym noAnn "a"))
    "[ { a } ]" ->> eApp noAnn (absN noAnn (eSym noAnn "a")) []
    "[ a b c ]" ->> eApp noAnn (eSym noAnn "a") [eSym noAnn "b", eSym noAnn "c"]
    "%"         ->> eVar noAnn (Left 1)
    "%1"        ->> eVar noAnn (Left 1)
    "%25"       ->> eVar noAnn (Left 25)

  describe "evaluation" $ do
    "a"         =>> eSym noAnn "a"
    "[ { a } ]" =>> eSym noAnn "a"
    "@nil"      =>> eNil noAnn

  describe "application with arguments" $ do
    "[ { % } a ]"           === "a"
    "[ { % } a b ]"         === "a"
    "[ { %1 } a b ]"        === "a"
    "[ { %2 } a b ]"        === "b"
    "[ [ { % } { % } ] a ]" === "a"

  describe "output" $ do
    "a"                             =*> []
    "[ @print-line ]"               =*> ["\n"]
    "[ @print-line a ]"             =*> ["a\n"]
    "[ @print-line Hello, world! ]" =*> ["Hello, world!\n"]

  describe "arrays" $ do
    "[ @array ]"       =>> eArr noAnn []
    "[ @array a b c ]" =>> eArr noAnn (map (eSym noAnn) (words "a b c"))

  describe "named parameters" $ do
    "{ a : @a }"   ->> absP noAnn ["a"] (eVar noAnn (Right "@a"))
    "{ a b : @a }" ->> absP noAnn ["a", "b"] (eVar noAnn (Right "@a"))

    "[ { a b : @b } x y ]"                === "y"
    "[ { a b : [ @b x ] } y { b : @b } ]" === "x"
    
    "[ [ { x : { y : [ @y @x ] } } foo ] @print-line ]" =*> ["foo\n"]

  describe "let" $ do
    "[ let a x : @a ]" ->> eApp noAnn (absN noAnn (eVar noAnn (Left 1))) [eSym noAnn "x"]
    "[ let a x b y : [ { [ %1 @b @a ] } @print-line ] ]" =*> ["y x\n"]
    "[ let x [ @array 1 2 ] : @x ]" === "[ @array 1 2 ]"

  describe "problems" $ do
    "@x" `hasProblem` NonexistentFreeVariable "@x"
    "%1" `hasProblem` NonexistentImplicitArgument 1
    "[ x ]" `hasProblem` Nonapplicable (eSym noAnn "x")

  describe "prompts" $ do
    shouldPromptAndBe "@x" (eNil noAnn) (eNil noAnn)
    shouldPromptAndBe "[ @array @x y ]" (eNil noAnn)
      (eArr noAnn [eNil noAnn, eSym noAnn "y"])

  describe "sequencing" $ do
    "[ @print-line a ] , [ @print-line b ]" =*> ["a\n", "b\n"]

------------------------------------------------------------------------

shouldParseTo :: String -> UxprRV -> Spec
shouldParseTo s e =
  it ("should parse `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e' -> (strip e') `shouldBe` e

expectParseFailure :: String -> Spec
expectParseFailure s =
  it ("fails on `" ++ s ++ "'") $
    case parse (words s) of
      Left _ -> return ()
      Right x -> expectationFailure ("Parsed to (" ++ show x ++ ")")

shouldEvaluateTo :: String -> UxprRV -> Spec
shouldEvaluateTo s v =
  it ("should evaluate `" ++ s ++ "'") $ do
    x <- parseAndEvaluate s
    (fmap strip x) `shouldBe` (Just v)

tryParsing :: String -> IO (Maybe UxprRV)
tryParsing s =
  case parse (words s) of
    Left err -> expectationFailure (show err) >> return Nothing
    Right e  -> return (Just e)

tryEvaluating :: UxprRV -> IO (Maybe UxprRV)
tryEvaluating e =
  case evaluate e of
    (Right e', []) -> return (Just e')
    (Left err, _)  -> expectationFailure (show err) >> return Nothing
    (Right _, _)   -> expectationFailure "Spurious output." >> return Nothing

parseAndEvaluate :: String -> IO (Maybe UxprRV)
parseAndEvaluate = tryParsing >=> maybe (return Nothing) tryEvaluating

shouldEvaluateEquivalently :: String -> String -> Spec
shouldEvaluateEquivalently a b =
  it ("should evaluate `" ++ a ++ "' like `" ++ b ++ "'") $
    do a' <- parseAndEvaluate a
       b' <- parseAndEvaluate b
       fmap strip a' `shouldBe` fmap strip b'

shouldOutput :: String -> [String] -> Spec
shouldOutput s xs =
  it ("should give output for `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e ->
        case evaluate e of
          (Left err, _) -> expectationFailure (show err)
          (Right _, xs') -> xs' `shouldBe` xs

hasProblem :: String -> Problem -> Spec
hasProblem s p =
  it ("should cause " ++ show p ++ " for `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e ->
        case evaluate e of
          (Right x, _) -> expectationFailure ("evaluated to " ++ show x)
          (Left p', _) -> p' `shouldBe` p

shouldPromptAndBe:: String -> UxprRV -> UxprRV -> Spec
shouldPromptAndBe s def expected =
  it ("retrying `" ++ s ++ "' w/ `" ++ show def ++ "' should give `"
      ++ show expected ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e ->
        case evaluateWithRestart (\_ k -> k def) e of
          (Right e', _) -> strip e' `shouldBe` expected
          (Left err, _) -> expectationFailure (show err)

strip :: UxprRV -> UxprRV
strip = mapAnns (const (Ann Nothing))
