 {-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Koko.Types where

import Bound (Scope, abstract, (>>>=))

import Control.Applicative
import Prelude.Extras (Eq1, Ord1, Show1, Read1)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Writer (Writer)
import Control.Monad (ap)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim (Parsec)

type Token       = String
type Token'      = (SourcePos, Token)
type Stream      = [Token']
type Parser a    = Parsec Stream () a
type Evaluator a = EitherT String (Writer [String]) a

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

type Variable = Either Int String
type Expr'    = Expr Variable
type Value'   = Value Variable

instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr
instance Read1 Expr

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

absWithImplicitParameters :: Eq a => Expr (Either Int a) -> Expr (Either Int a)
absWithImplicitParameters = EVal . VAbs . abstract (either Just (const Nothing))

absN :: Expr' -> Expr'
absN = absWithImplicitParameters
