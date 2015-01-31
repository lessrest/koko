module Koko where

import Koko.Types

import Bound (instantiate)
import Control.Applicative
import Control.Monad.Trans.Either (runEitherT, left)
import Control.Monad.Writer (tell, runWriter)

evaluate :: Expr' -> (Either String Expr', [String])
evaluate e = runWriter (runEitherT (evaluate' e))

evaluate' :: Expr' -> Evaluator Expr'
evaluate' (EVal (VSym s)) = pure (EVal (VSym s))
evaluate' (EVal (VAbs e)) = pure (EVal (VAbs e))
evaluate' (EVal (VFun s)) = pure (EVal (VFun s))
evaluate' (EApp e xs) = flip apply xs =<< evaluate' e
evaluate' (EVar (Right v)) = pure (EVal (VFun v))
evaluate' (EVar (Left v)) = left $ "Unknown variable " ++ show v
evaluate' e = left $ "Evaluation error: " ++ show e

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
  vs <- mapM evaluate' es
  let f i = vs !! (i - 1)
  evaluate' (instantiate f e)
apply (EVal (VFun s)) es = do
  f <- maybe (left $ "No such function " ++ s) pure (lookup s functions)
  vs <- mapM evaluate' es
  f vs
apply _ _ = left "Application error"
