{-# LANGUAGE LambdaCase #-}
module Koko.Friendly where

import System.IO

import Koko.Types

showProblem :: Problem -> String
showProblem =
  \case NonexistentImplicitArgument i ->
          "The implicit argument %" ++ show i ++ " is not bound."
        NonexistentFreeVariable s ->
          "The free variable " ++ s ++ " has no definition."
        Nonapplicable v ->
          "The value `" ++ show v ++ "' is not a function."
        UnknownError ->
          "Unknown error."

showPrompt :: Problem -> IO ()
showPrompt p =
  do putStrLn $ "Error: " ++ showProblem p
     putStr "Type an expression: "
     hFlush stdout
