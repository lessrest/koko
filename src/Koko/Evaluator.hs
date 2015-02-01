{-# LANGUAGE LambdaCase, GADTs, RankNTypes #-}
module Koko.Evaluator where

import Koko.Types
import qualified Koko.Friendly as Friendly

import Bound (instantiate)
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Prompt
import Control.Monad.Trans.Either (runEitherT, left)
import Control.Monad.State (evalStateT, put, get)
import Control.Monad.Writer (Writer, tell, runWriter)

prepare :: UxprRV -> ExecM UxprRV
prepare (U a u) =
  do _ <- xAnn a
     case u of
       ENil -> xNil
       EVar (Left v) -> xIdx v
       EVar (Right v) -> xName v
       ESym s -> xSym s
       EAbs e -> xAbs e
       EFun v -> xName v
       EArr es -> xArr es
       EApp e xs -> xApp e xs
       ESeq es -> last <$> mapM prepare es

execute :: ExecM UxprRV -> Evaluator'
execute = iterM run
  where
    run :: Exec Evaluator' -> Evaluator'
    run = \case
      XAnn a f  -> put a >> f (error "XAnn value should be ignored")
      XHalt p   -> lift (left p)
      XIdx i f  -> f =<< problem (NonexistentImplicitArgument i)
      XName n f -> case lookup n functions of
                     Just _  -> f (eFun noAnn n)
                     Nothing ->
                       case lookup n globals of
                         Just v  -> f v
                         Nothing -> problem (NonexistentFreeVariable n)
      XNil f    -> f (eNil noAnn)
      XSym s f  -> f (eSym noAnn s)
      XAbs x f  -> f (eAbs noAnn x)
      XArr es f -> f =<< eArr noAnn <$> mapM (execute . prepare) es
      XApp e es f ->
        do e' <- evaluate' e
           es' <- mapM evaluate' es
           f =<< apply e' es'

type Result = (Either Problem UxprRV, [String])

evaluate :: UxprRV -> Result
evaluate = evaluateWithRestart (\p _ -> return (Left p))

runAsWriter :: Free Action (Either Problem UxprRV) -> Result
runAsWriter = runWriter . iterM run
  where
    run :: Action (Writer [String] (Either Problem UxprRV))
        -> Writer [String] (Either Problem UxprRV)
    run (DoPrint es m) = tell [Friendly.showExprs es ++ "\n"] >> m
    run (DoPrompt ann _ f) = f (eNil ann)

runAsIO :: Free Action (Either Problem UxprRV) -> IO (Either Problem UxprRV)
runAsIO = iterM run
  where
    run :: Action (IO (Either Problem UxprRV)) -> IO (Either Problem UxprRV)
    run (DoPrint e m) = putStrLn (Friendly.showExprs e) >> m
    run (DoPrompt ann p f) = f =<< Friendly.showPrompt ann p evaluateIO

evaluateWithRestart
  :: (Problem -> (UxprRV -> PromptResult ActionM) -> PromptResult ActionM)
  -> UxprRV
  -> Result
evaluateWithRestart f =
  runAsWriter .
  runPromptT return (\(UncaughtProblem _ p) k -> f p k) (>>=) .
  runEitherT .
  flip evalStateT (Ann Nothing) .
  evaluate'

evaluateIO :: UxprRV -> IO (Either Problem UxprRV)
evaluateIO =
  runAsIO .
  runPromptT return (\(UncaughtProblem ann p) k -> doPrompt ann p >>= k) (>>=) .
  runEitherT .
  flip evalStateT (Ann Nothing) .
  evaluate'
        
evaluate' :: UxprRV -> Evaluator'
evaluate' = execute . prepare

functions :: [(String, [UxprRV] -> Evaluator')]
functions = [("@print-line", printLine),
             ("@array", doArray)]
  where
    printLine v = lift . lift . lift $ do
      doPrint v
      pure (eNil noAnn)
    doArray v = return (eArr noAnn v)

globals :: [(String, UxprRV)]
globals = [("@nil", eNil noAnn)]

apply :: UxprRV -> [UxprRV] -> Evaluator'
apply (U _ (EAbs e)) es = do
  let f i = es !! (i - 1)
  evaluate' (instantiate f e)
apply (U _ (EFun s)) es = do
  case lookup s functions of
    Nothing -> problem (NonexistentFreeVariable s)
    Just f -> f es
apply e _ = problem (Nonapplicable e)

problem :: Problem -> Evaluator'
problem p = get >>= \a -> lift . lift . prompt $ UncaughtProblem a p
