{-# LANGUAGE LambdaCase, GADTs, RankNTypes #-}
module Koko.Evaluator where

import Koko.Types
import qualified Koko.UFriendly as Friendly

import Bound (instantiate)
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Prompt
import Control.Monad.Trans.Either (runEitherT, left)
import Control.Monad.Writer (Writer, tell, runWriter)

prepare :: UxprRV -> UxecM UxprRV
prepare (U ann u) =
  case u of
    UNil -> uxNil
    UVar (Left v) -> uxIdx v
    UVar (Right v) -> uxName v
    USym s -> uxSym s
    UAbs e -> uxAbs e
    UFun v -> uxName v
    UArr es -> uxArr es
    UApp e xs -> uxApp e xs
    USeq es -> last <$> mapM prepare es

execute :: UxecM UxprRV -> Uvaluator'
execute = iterM run
  where
    run :: Uxec Uvaluator' -> Uvaluator'
    run = \case
      UxHalt p   -> left p
      UxIdx i f  -> f =<< problem (NonexistentImplicitArgument i)
      UxName n f -> case lookup n functions of
                      Just _  -> f (uFun noAnn n)
                      Nothing ->
                        case lookup n globals of
                          Just v  -> f v
                          Nothing -> problem (NonexistentFreeVariable n)
      UxNil f    -> f (uNil noAnn)
      UxSym s f  -> f (uSym noAnn s)
      UxAbs x f  -> f (uAbs noAnn x)
      UxArr es f -> f =<< uArr noAnn <$> mapM (execute . prepare) es
      UxApp e es f ->
        do e' <- evaluate' e
           es' <- mapM evaluate' es
           f =<< apply e' es'

type Result = (Either Uroblem UxprRV, [String])

evaluate :: UxprRV -> Result
evaluate = evaluateWithRestart (\p _ -> return (Left p))

runAsWriter :: Free Uction (Either Uroblem UxprRV) -> Result
runAsWriter = runWriter . iterM run
  where
    run :: Uction (Writer [String] (Either Uroblem UxprRV))
        -> Writer [String] (Either Uroblem UxprRV)
    run (UDoPrint es m) = tell [Friendly.showExprs es ++ "\n"] >> m
    run (UDoPrompt _ f) = f (uNil noAnn)

runAsIO :: Free Uction (Either Uroblem UxprRV) -> IO (Either Uroblem UxprRV)
runAsIO = iterM run
  where
    run :: Uction (IO (Either Uroblem UxprRV)) -> IO (Either Uroblem UxprRV)
    run (UDoPrint e m) = putStrLn (Friendly.showExprs e) >> m
    run (UDoPrompt p f) = f =<< Friendly.showPrompt p evaluateIO

evaluateWithRestart
  :: (Uroblem -> (UxprRV -> UromptResult UctionM) -> UromptResult UctionM)
  -> UxprRV
  -> Result
evaluateWithRestart f =
  runAsWriter .
  runPromptT return (\(UncaughtProblem p) k -> f p k) (>>=) .
  runEitherT .
  evaluate'

evaluateIO :: UxprRV -> IO (Either Uroblem UxprRV)
evaluateIO =
  runAsIO .
  runPromptT return (\(UncaughtProblem p) k -> uDoPrompt p >>= k) (>>=) .
  runEitherT .
  evaluate'
        
evaluate' :: UxprRV -> Uvaluator'
evaluate' = execute . prepare

functions :: [(String, [UxprRV] -> Uvaluator')]
functions = [("@print-line", printLine),
             ("@array", doArray)]
  where
    printLine v = lift . lift $ do
      uDoPrint v
      pure (uNil noAnn)
    doArray v = return (uArr noAnn v)

globals :: [(String, UxprRV)]
globals = [("@nil", uNil noAnn)]

apply :: UxprRV -> [UxprRV] -> Uvaluator'
apply (U _ (UAbs e)) es = do
  let f i = es !! (i - 1)
  evaluate' (instantiate f e)
apply (U _ (UFun s)) es = do
  case lookup s functions of
    Nothing -> problem (NonexistentFreeVariable s)
    Just f -> f es
apply e _ = problem (Nonapplicable e)

problem :: Uroblem -> Uvaluator'
problem = lift . prompt . UncaughtProblem
