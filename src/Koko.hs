{-# LANGUAGE LambdaCase, GADTs #-}
module Koko where

import Koko.Types

import Bound (instantiate)
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Prompt
import Control.Monad.Trans.Either (runEitherT, left)
import Control.Monad.Writer (tell, runWriterT)

prepare :: Expr' -> ExecM Expr'
prepare =
  \case
    EVal VNil -> xNil
    EVar (Left v) -> xIdx v
    EVar (Right v) -> xName v
    EVal (VSym s) -> xSym s
    EVal (VAbs e) -> xAbs e
    EVal (VFun v) -> xName v
    EVal (VArr es) -> xArr es
    EApp e xs -> xApp e xs

execute :: ExecM Expr' -> Evaluator Expr'
execute = iterM run
  where
    run :: Exec (Evaluator Expr') -> Evaluator Expr'
    run = \case
      XHalt p   -> left p
      XIdx i _  -> left (NonexistentImplicitArgument i)
      XName n f -> case lookup n functions of
                     Nothing -> left (NonexistentFreeVariable n)
                     Just _  -> f (EVal (VFun n))
      XNil f    -> f (EVal (VNil))
      XSym s f  -> f (EVal (VSym s))
      XAbs x f  -> f (EVal (VAbs x))
      XArr es f -> f =<< EVal . VArr <$> mapM (execute . prepare) es
      XApp e es f ->
        do e' <- evaluate' e
           es' <- mapM evaluate' es
           f =<< apply e' es'

evaluate :: Expr' -> (Either Problem Expr', [String])
evaluate e = runPromptC id (\(UncaughtProblem p) _ -> (Left p, []))
               (runWriterT (runEitherT (evaluate' e)))

evaluate' :: Expr' -> Evaluator Expr'
evaluate' = execute . prepare

functions :: [(String, [Expr'] -> Evaluator Expr')]
functions = [("@print-line", doPrint),
             ("@array", doArray)]
  where
    doPrint v = tell [unwords (map output v) ++ "\n"] >> pure (EVal VNil)
    doArray v = return (EVal (VArr v))

output :: Expr' -> String
output (EVal (VSym s)) = s
output (EVal _) = "<value>"
output _ = "<unevaluated>"

apply :: Expr' -> [Expr'] -> Evaluator Expr'
apply (EVal (VAbs e)) es = do
  let f i = es !! (i - 1)
  evaluate' (instantiate f e)
apply (EVal (VFun s)) es = do
  f <- maybe (left $ NonexistentFreeVariable s) pure (lookup s functions)
  f es
apply (EVal v) _ = left (Nonapplicable v)
