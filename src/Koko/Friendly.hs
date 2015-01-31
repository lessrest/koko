{-# LANGUAGE LambdaCase #-}
module Koko.Friendly where

import System.Console.Haskeline

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

showPrompt :: Problem -> IO Expr'
showPrompt p = runInputT defaultSettings $
  do outputStrLn $ ":: Error: " ++ showProblem p
     getInputLine ":: Use value: " >>=
       \case Nothing -> return (EVal VNil)
             Just s  -> return (read s)
