{-# LANGUAGE LambdaCase, GADTs, RankNTypes #-}
module Koko where

import Koko.Types
import qualified Koko.Friendly as Friendly

import Bound (instantiate)
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Prompt
import Control.Monad.Trans.Either (runEitherT, left)
import Control.Monad.Writer (Writer, tell, runWriter)

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

execute :: ExecM Expr' -> Evaluator'
execute = iterM run
  where
    run :: Exec Evaluator' -> Evaluator'
    run = \case
      XHalt p   -> left p
      XIdx i f  -> f =<< problem (NonexistentImplicitArgument i)
      XName n f -> case lookup n functions of
                     Just _  -> f (EVal (VFun n))
                     Nothing ->
                       case lookup n globals of
                         Just v  -> f (EVal v)
                         Nothing -> problem (NonexistentFreeVariable n)
      XNil f    -> f (EVal (VNil))
      XSym s f  -> f (EVal (VSym s))
      XAbs x f  -> f (EVal (VAbs x))
      XArr es f -> f =<< EVal . VArr <$> mapM (execute . prepare) es
      XApp e es f ->
        do e' <- evaluate' e
           es' <- mapM evaluate' es
           f =<< apply e' es'

type Result = (Either Problem Expr', [String])

evaluate :: Expr' -> Result
evaluate = evaluateWithRestart (\p _ -> return (Left p))

runAsWriter :: Free Action (Either Problem Expr') -> Result
runAsWriter = runWriter . iterM run
  where
    run :: Action (Writer [String] (Either Problem Expr'))
        -> Writer [String] (Either Problem Expr')
    run (DoPrint es m) = tell [Friendly.showExprs es ++ "\n"] >> m
    run (DoPrompt _ f) = f (EVal VNil)

runAsIO :: Free Action (Either Problem Expr') -> IO (Either Problem Expr')
runAsIO = iterM run
  where
    run :: Action (IO (Either Problem Expr')) -> IO (Either Problem Expr')
    run (DoPrint e m) = putStrLn (Friendly.showExprs e) >> m
    run (DoPrompt p f) = f =<< Friendly.showPrompt p evaluateIO

evaluateWithRestart
  :: (Problem -> (Expr' -> PromptResult Base) -> PromptResult Base)
  -> Expr'
  -> Result
evaluateWithRestart f =
  runAsWriter .
  runPromptT return (\(UncaughtProblem p) k -> f p k) (>>=) .
  runEitherT .
  evaluate'

evaluateIO :: Expr' -> IO (Either Problem Expr')
evaluateIO =
  runAsIO .
  runPromptT return (\(UncaughtProblem p) k -> doPrompt p >>= k) (>>=) .
  runEitherT .
  evaluate'
        
evaluate' :: Expr' -> Evaluator'
evaluate' = execute . prepare

functions :: [(String, [Expr'] -> Evaluator')]
functions = [("@print-line", printLine),
             ("@array", doArray)]
  where
    printLine v = lift . lift $ do
      doPrint v
      pure (EVal VNil)
    doArray v = return (EVal (VArr v))

globals :: [(String, Value')]
globals = [("@nil", VNil)]

apply :: Expr' -> [Expr'] -> Evaluator'
apply (EVal (VAbs e)) es = do
  let f i = es !! (i - 1)
  evaluate' (instantiate f e)
apply (EVal (VFun s)) es = do
  case lookup s functions of
    Nothing -> problem (NonexistentFreeVariable s)
    Just f -> f es
apply (EVal v) _ = problem (Nonapplicable v)
apply _ _ = problem UnknownError

problem :: Problem -> Evaluator'
problem = lift . prompt . UncaughtProblem
