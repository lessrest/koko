{-# LANGUAGE LambdaCase #-}
module Koko.Friendly where

import Bound (unscope, Var (..))
import Control.Monad.Trans (lift)
import System.Console.Haskeline
import Text.PrettyPrint.ANSI.Leijen

import Koko.Types
import Koko.Parser (parse)

showProblem :: Ann -> Problem -> Doc
showProblem ann p = text err <+> annotation
  where
    annotation = case ann of
                   Ann Nothing -> text "No source location."
                   Ann (Just i) -> text ("At token " ++ show i ++ ".")
    err = case p of
            NonexistentImplicitArgument i ->
              "The implicit argument %" ++ show i ++ " is not bound."
            NonexistentFreeVariable s ->
              "The free variable " ++ s ++ " has no definition."
            Nonapplicable v ->
              "The value `" ++ show v ++ "' is not a function."
            UnknownError ->
              "Unknown error."

showPrompt :: Ann -> Problem -> (UxprRV -> IO (Either Problem UxprRV)) -> IO UxprRV
showPrompt ann p eval = runInputT defaultSettings (showPrompt' ann p eval)

showPrompt' :: (Monad m, MonadException m)
            => Ann -> Problem -> (UxprRV -> m (Either Problem UxprRV))
            -> InputT m UxprRV
showPrompt' ann p eval = do
  outputStrLn . showError . showDoc . showProblem ann $ p
  getInputLine showUseValuePrompt >>=
    \case Nothing -> return (eNil noAnn)
          Just s ->
            case parse (words s) of
              Left err -> do outputStrLn . showError $ show err
                             showPrompt' noAnn p eval
              Right e ->
                lift (eval e) >>= \case
                  Left p' -> showPrompt' noAnn p' eval
                  Right e' -> return e'

showError :: String -> String
showError s = showDoc $ bold (red $ text "Error!") <+> text s

showUseValuePrompt :: String
showUseValuePrompt = showDoc $ bold (text "Use value: ")

showDoc :: Doc -> String
showDoc d = displayS (renderPretty 0.8 72 d) ""
  
showExpr :: UxprRV -> String
showExpr e = showDoc (renderExpr e)

showExprs :: [UxprRV] -> String
showExprs es = displayS (renderPretty 0.8 72 (hsep (map renderExpr es))) ""

renderExpr' :: (a -> Doc) -> UxprR a -> Doc
renderExpr' var (U _ u) = case u of
  EVar v -> var v
  EApp e es -> text "[" <+> sep (map (renderExpr' var) (e:es)) <+> text "]"
  ESeq es -> sep . punctuate (text " ,") . map (renderExpr' var) $ es
  ESym s -> text s
  EFun s -> bold $ text s
  ENil -> red $ text "@nil"
  EArr es -> text "[" <+> hang 2 (sep ((bold . yellow $ text "@array") :
                                       map (renderExpr' var) es))
                      <+> text "]"
  EAbs s -> text "{" <+> hang 2 (renderExpr' (renderScopeVar var) (unscope s))
                     <+> text "}"


renderExpr :: UxprRV -> Doc
renderExpr = renderExpr' renderVar

renderVar :: Variable -> Doc
renderVar = \case
  Left i  -> green     . text $ "%" ++ show i
  Right s -> dullgreen . text $ s

renderScopeVar :: (a -> Doc) -> Var Int (UxprR a) -> Doc
renderScopeVar var = \case
  B i -> renderVar (Left i)
  F e -> renderExpr' var e
