{-# LANGUAGE LambdaCase #-}
module Koko.Friendly where

import Bound (unscope, Var (..))
import System.Console.Haskeline
import Text.PrettyPrint.ANSI.Leijen

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

showExpr :: Expr' -> String
showExpr e = displayS (renderPretty 0.8 72 (renderExpr e)) ""

showExprs :: [Expr'] -> String
showExprs es = displayS (renderPretty 0.8 72 (hsep (map renderExpr es))) ""

renderExpr' :: (a -> Doc) -> Expr a -> Doc
renderExpr' var = \case
  EVar v -> var v
  EVal v -> renderVal var v
  EApp e es -> text "[" <+> sep (map (renderExpr' var) (e:es)) <+> text "]"

renderExpr :: Expr' -> Doc
renderExpr = renderExpr' renderVar

renderVar :: Variable -> Doc
renderVar = \case
  Left i  -> green     . text $ "%" ++ show i
  Right s -> dullgreen . text $ s

renderScopeVar :: (a -> Doc) -> Var Int (Expr a) -> Doc
renderScopeVar var = \case
  B i -> renderVar (Left i)
  F e -> renderExpr' var e

renderVal :: (a -> Doc) -> Value a -> Doc
renderVal var = \case
  VSym s -> text s
  VFun s -> bold $ text s
  VNil -> red $ text "@nil"
  VArr es -> text "[" <+> hang 2 (sep ((bold . yellow $ text "@array") :
                                       map (renderExpr' var) es))
                      <+> text "]"
  VAbs s -> text "{" <+> hang 2 (renderExpr' (renderScopeVar var) (unscope s))
                     <+> text "}"
