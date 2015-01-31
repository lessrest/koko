{-#
  LANGUAGE
    DeriveFoldable,
    DeriveFunctor,
    DeriveTraversable,
    FlexibleContexts,
    GADTs,
    TemplateHaskell #-}

module Koko.Types where

import Bound (Scope, abstract, (>>>=))

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Prompt

import Prelude.Extras (Eq1, Ord1, Show1, Read1)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Writer (Writer)
import Control.Monad (ap)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.List (elemIndex)

import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim (Parsec)

type Token    = String
type Token'   = (SourcePos, Token)
type Stream   = [Token']
type Parser a = Parsec Stream () a

data Restart a where
  UncaughtProblem :: Problem -> Restart Expr'

type Evaluator a = EitherT Problem (PromptT Restart (Writer [String])) a

type PromptResult = Writer [String] (Either Problem Expr')

data Expr v = EVar v
            | EApp (Expr v) [Expr v]
            | EVal (Value v)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data Value v = VSym String
             | VAbs (Scope Int Expr v)
             | VFun String
             | VNil
             | VArr [Expr v]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = EVar
  (<*>) = ap

instance Monad Expr where
  return = EVar
  e >>= f =
    case e of
     EVar v -> f v
     EApp e' es -> EApp (e' >>= f) (map (>>= f) es)
     EVal (VAbs e') -> EVal (VAbs (e' >>>= f))
     EVal (VSym x) -> EVal (VSym x)
     EVal (VFun s) -> EVal (VFun s)
     EVal VNil -> EVal VNil
     EVal (VArr es) -> EVal (VArr (map (>>= f) es))

type Variable = Either Int String
type Expr'    = Expr Variable
type Value'   = Value Variable

instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr
instance Read1 Expr

data Problem = NonexistentImplicitArgument Int
             | NonexistentFreeVariable String
             | Nonapplicable Value'
             | UnknownError
  deriving (Eq, Ord, Show)

data Exec a where
  XHalt :: Problem -> Exec a
  XNil  :: (Expr' -> a) -> Exec a
  XName :: String -> (Expr' -> a) -> Exec a
  XIdx  :: Int -> (Expr' -> a) -> Exec a
  XSym  :: String -> (Expr' -> a) -> Exec a
  XAbs  :: Scope Int Expr Variable -> (Expr' -> a) -> Exec a
  XApp  :: Expr' -> [Expr'] -> (Expr' -> a) -> Exec a
  XArr  :: [Expr'] -> (Expr' -> a) -> Exec a
  deriving Functor

$(makeFree ''Exec)

type ExecM = Free Exec

absWithImplicitParameters :: Eq a => Expr (Either Int a) -> Expr (Either Int a)
absWithImplicitParameters = EVal . VAbs . abstract (either Just (const Nothing))

absN :: Expr' -> Expr'
absN = absWithImplicitParameters

absWithExplicitParameters :: Eq a => [a] -> Expr (Either Int a) -> Expr (Either Int a)
absWithExplicitParameters ps = EVal . VAbs . abstract (either (const Nothing) f)
  where f x = (+ 1) <$> elemIndex x ps

absP :: [String] -> Expr' -> Expr'
absP ps = absWithExplicitParameters (map ('@':) ps)
