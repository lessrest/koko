{-# LANGUAGE GADTs #-}
module Main where

import Test.Hspec

import Koko
import Koko.Types
import Koko.Parser

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
    "@"         ->> EVar (Right "@")
    "@foo"      ->> EVar (Right "@foo")
    "{ }"       ->> absN (EVal VNil)
    "{ a }"     ->> absN (EVal (VSym "a"))
    "{ { a } }" ->> absN (absN (EVal (VSym "a")))
    "[ { a } ]" ->> EApp (absN (EVal (VSym "a"))) []
    "[ a b c ]" ->> EApp (EVal (VSym "a")) [EVal (VSym "b"), EVal (VSym "c")]
    "%"         ->> EVar (Left 1)
    "%1"        ->> EVar (Left 1)
    "%25"       ->> EVar (Left 25)

  describe "evaluation" $ do
    "a"         =>> EVal (VSym "a")
    "[ { a } ]" =>> EVal (VSym "a")
    "@nil"      =>> EVal VNil

  describe "application with arguments" $ do
    "[ { % } a ]"           =>> EVal (VSym "a")
    "[ { % } a b ]"         =>> EVal (VSym "a")
    "[ { %1 } a b ]"        =>> EVal (VSym "a")
    "[ { %2 } a b ]"        =>> EVal (VSym "b")
    "[ [ { % } { % } ] a ]" =>> EVal (VSym "a")

  describe "output" $ do
    "a"                             =*> []
    "[ @print-line ]"               =*> ["\n"]
    "[ @print-line a ]"             =*> ["a\n"]
    "[ @print-line Hello, world! ]" =*> ["Hello, world!\n"]

  describe "arrays" $ do
    "[ @array ]"       =>> EVal (VArr [])
    "[ @array a b c ]" =>> EVal (VArr (map (EVal . VSym) (words "a b c")))

  describe "named parameters" $ do
    "{ a : @a }"   ->> absP ["a"] (EVar (Right "@a"))
    "{ a b : @a }" ->> absP ["a", "b"] (EVar (Right "@a"))

    "[ { a b : @b } x y ]"                =>> EVal (VSym "y")
    "[ { a b : [ @b x ] } y { b : @b } ]" =>> EVal (VSym "x")
    
    "[ [ { x : { y : [ @y @x ] } } foo ] @print-line ]" =*> ["foo\n"]

  describe "let" $ do
    "[ let a x : @a ]" ->> EApp (absN (EVar (Left 1))) [EVal (VSym "x")]
    "[ let a x b y : [ { [ %1 @b @a ] } @print-line ] ]" =*> ["y x\n"]

  describe "problems" $ do
    "@x" `hasProblem` NonexistentFreeVariable "@x"
    "%1" `hasProblem` NonexistentImplicitArgument 1
    "[ x ]" `hasProblem` Nonapplicable (VSym "x")

  describe "prompts" $ do
    shouldPromptAndBe "@x" (EVal VNil) (EVal VNil)
    shouldPromptAndBe "[ @array @x y ]" (EVal (VNil))
      (EVal (VArr [EVal (VNil), EVal (VSym "y")]))

------------------------------------------------------------------------

shouldParseTo :: String -> Expr' -> Spec
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

shouldEvaluateTo :: String -> Expr' -> Spec
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

hasProblem :: String -> Problem -> Spec
hasProblem s p =
  it ("should cause " ++ show p ++ " for `" ++ s ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e ->
        case evaluate e of
          (Right x, _) -> expectationFailure ("evaluated to " ++ show x)
          (Left p', _) -> p' `shouldBe` p

shouldPromptAndBe:: String -> Expr' -> Expr' -> Spec
shouldPromptAndBe s def expected =
  it ("retrying `" ++ s ++ "' w/ `" ++ show def ++ "' should give `"
      ++ show expected ++ "'") $
    case parse (words s) of
      Left err -> expectationFailure (show err)
      Right e ->
        case evaluateWithRestart (\_ k -> k def) e of
          (Right e', _) -> e' `shouldBe` expected
          (Left err, _) -> expectationFailure (show err)
