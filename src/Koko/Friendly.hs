{-# LANGUAGE LambdaCase #-}
module Koko.Friendly where

import Bound (unscope, Var (..))
import Control.Monad.Trans (lift)
import System.Console.Haskeline
import Text.PrettyPrint.ANSI.Leijen

import Koko.Types
import Koko.Parser (parse)

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

showPrompt :: Problem -> (Expr' -> IO (Either Problem Expr')) -> IO Expr'
showPrompt p eval = runInputT defaultSettings (showPrompt' p eval)

showPrompt' :: (Monad m, MonadException m)
            => Problem -> (Expr' -> m (Either Problem Expr')) -> InputT m Expr'
showPrompt' p eval = do
  outputStrLn . showError . showProblem $ p
  getInputLine showUseValuePrompt >>=
    \case Nothing -> return (EVal VNil)
          Just s ->
            case parse (words s) of
              Left err -> do outputStrLn . showError $ show err
                             showPrompt' p eval
              Right e ->
                lift (eval e) >>= \case
                  Left p' -> showPrompt' p' eval
                  Right e' -> return e'

showError :: String -> String
showError s = showDoc $ bold (red $ text "Error!") <+> text s

showUseValuePrompt :: String
showUseValuePrompt = showDoc $ bold (text "Use value: ")

showDoc :: Doc -> String
showDoc d = displayS (renderPretty 0.8 72 d) ""
  
showExpr :: Expr' -> String
showExpr e = showDoc (renderExpr e)

showExprs :: [Expr'] -> String
showExprs es = displayS (renderPretty 0.8 72 (hsep (map renderExpr es))) ""

renderExpr' :: (a -> Doc) -> Expr a -> Doc
renderExpr' var = \case
  EVar v -> var v
  EVal v -> renderVal var v
  EApp e es -> text "[" <+> sep (map (renderExpr' var) (e:es)) <+> text "]"
  ESeq es -> sep . punctuate (text " ,") . map (renderExpr' var) $ es

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
