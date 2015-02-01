{-# LANGUAGE LambdaCase #-}
module Koko.UFriendly where

import Bound (unscope, Var (..))
import Control.Monad.Trans (lift)
import System.Console.Haskeline
import Text.PrettyPrint.ANSI.Leijen

import Koko.Types
import Koko.UParser (parse)

showProblem :: Uroblem -> String
showProblem =
  \case NonexistentImplicitArgument i ->
          "The implicit argument %" ++ show i ++ " is not bound."
        NonexistentFreeVariable s ->
          "The free variable " ++ s ++ " has no definition."
        Nonapplicable v ->
          "The value `" ++ show v ++ "' is not a function."
        UnknownError ->
          "Unknown error."

showPrompt :: Uroblem -> (UxprRV -> IO (Either Uroblem UxprRV)) -> IO UxprRV
showPrompt p eval = runInputT defaultSettings (showPrompt' p eval)

showPrompt' :: (Monad m, MonadException m)
            => Uroblem -> (UxprRV -> m (Either Uroblem UxprRV)) -> InputT m UxprRV
showPrompt' p eval = do
  outputStrLn . showError . showProblem $ p
  getInputLine showUseValuePrompt >>=
    \case Nothing -> return (uNil noAnn)
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
  
showExpr :: UxprRV -> String
showExpr e = showDoc (renderExpr e)

showExprs :: [UxprRV] -> String
showExprs es = displayS (renderPretty 0.8 72 (hsep (map renderExpr es))) ""

renderExpr' :: (a -> Doc) -> UxprR a -> Doc
renderExpr' var (U _ u) = case u of
  UVar v -> var v
  UApp e es -> text "[" <+> sep (map (renderExpr' var) (e:es)) <+> text "]"
  USeq es -> sep . punctuate (text " ,") . map (renderExpr' var) $ es
  USym s -> text s
  UFun s -> bold $ text s
  UNil -> red $ text "@nil"
  UArr es -> text "[" <+> hang 2 (sep ((bold . yellow $ text "@array") :
                                       map (renderExpr' var) es))
                      <+> text "]"
  UAbs s -> text "{" <+> hang 2 (renderExpr' (renderScopeVar var) (unscope s))
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
