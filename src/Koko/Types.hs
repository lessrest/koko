{-#
  LANGUAGE
    DeriveFoldable,
    DeriveFunctor,
    DeriveTraversable,
    FlexibleContexts,
    GADTs,
    TemplateHaskell,
    StandaloneDeriving,
    LambdaCase,
    TypeSynonymInstances,
    FlexibleInstances
 #-}

module Koko.Types where

import Bound (Scope, abstract, (>>>=))

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Prompt

import Prelude.Extras (Eq1, Ord1, Show1, Read1)
import Control.Monad.Trans.Either (EitherT)
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

data Uxpr f s v =
    UVar v
  | UApp f [f]
  | UAbs s
  | USeq [f]
  | USym String
  | UFun String
  | UNil
  | UArr [f]
  deriving (Eq, Ord, Show, Read, Functor)

uVar :: Ann -> v -> UxprR v
uVar ann = U ann . UVar

uApp :: Ann -> UxprR v -> [UxprR v] -> UxprR v
uApp ann u us = U ann (UApp u us)

uAbs :: Ann -> Scope Int UxprR v -> UxprR v
uAbs ann = U ann . UAbs

uSeq :: Ann -> [UxprR v] -> UxprR v
uSeq ann = U ann . USeq

uSym :: Ann -> String -> UxprR v
uSym ann = U ann . USym

uFun :: Ann -> String -> UxprR v
uFun ann = U ann . UFun

uNil :: Ann -> UxprR v
uNil ann = U ann UNil

uArr :: Ann -> [UxprR v] -> UxprR v
uArr ann = U ann . UArr

newtype Ann = Ann (Maybe Int)
  deriving (Eq, Ord, Show, Read)

noAnn :: Ann
noAnn = Ann Nothing

data UxprR' a v = U a (Uxpr (UxprR' a v) (Scope Int (UxprR' a) v) v)
type UxprR = UxprR' Ann
type UxprRV = UxprR Variable

deriving instance Eq v => Eq (UxprR v)
deriving instance Ord v => Ord (UxprR v)
deriving instance Show v => Show (UxprR v)
deriving instance Read v => Read (UxprR v)

instance Functor UxprR where
  fmap f (U ann uxpr) =
    case uxpr of
      UVar v -> U ann (UVar (f v))
      UApp u us -> U ann (UApp (fmap f u) (map (fmap f) us))
      USeq us -> U ann (USeq (map (fmap f) us))
      USym s -> U ann (USym s)
      UFun s -> U ann (UFun s)
      UAbs s -> U ann (UAbs (fmap f s))
      UNil -> U ann UNil
      UArr us -> U ann (UArr (map (fmap f) us))

instance Monad UxprR where
  return = U (Ann Nothing) . UVar
  U ann e >>= f =
    case e of
      UVar v -> f v
      UApp u us -> U ann (UApp (u >>= f) (map (>>= f) us))
      USeq us -> U ann (USeq (map (>>= f) us))
      USym s -> U ann (USym s)
      UFun s -> U ann (UFun s)
      UAbs s -> U ann (UAbs (s >>>= f))
      UNil -> U ann UNil
      UArr us -> U ann (UArr (map (>>= f) us))

instance Applicative UxprR where
  pure = U (Ann Nothing) . UVar
  (<*>) = ap

instance Eq1 UxprR
instance Ord1 UxprR
instance Show1 UxprR
instance Read1 UxprR

data Restart e a where
  UncaughtProblem :: Problem' e -> Restart e e

type Evaluator m a = EitherT (Problem' a) (PromptT (Restart a) m) a
type PromptResult' m e = m (Either (Problem' e) e)
type PromptResult m = PromptResult' m Expr'
type UromptResult m = m (Either Uroblem UxprRV)

data Expr v = EVar v
            | EApp (Expr v) [Expr v]
            | EVal (Value v)
            | ESeq [Expr v]
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
     ESeq es -> ESeq (map (>>= f) es)
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

data Problem' e = NonexistentImplicitArgument Int
                | NonexistentFreeVariable String
                | Nonapplicable e
                | UnknownError
  deriving (Eq, Ord, Show)

type Problem = Problem' Expr'
type Uroblem = Problem' UxprRV

data Action a where
  DoPrint :: [Expr'] -> a -> Action a
  DoPrompt :: Problem -> (Expr' -> a) -> Action a
  deriving Functor

$(makeFree ''Action)

type ActionM = Free Action
type Base = ActionM

data Uction a where
  UDoPrint :: [UxprRV] -> a -> Uction a
  UDoPrompt :: Uroblem -> (UxprRV -> a) -> Uction a
  deriving Functor

$(makeFree ''Uction)

type UctionM = Free Uction

type Evaluator' = Evaluator Base Expr'
type Uvaluator' = Evaluator UctionM UxprRV

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

data Uxec a where
  UxHalt :: Uroblem -> Uxec a
  UxNil  :: (UxprRV -> a) -> Uxec a
  UxName :: String -> (UxprRV -> a) -> Uxec a
  UxIdx  :: Int -> (UxprRV -> a) -> Uxec a
  UxSym  :: String -> (UxprRV -> a) -> Uxec a
  UxAbs  :: Scope Int UxprR Variable -> (UxprRV -> a) -> Uxec a
  UxApp  :: UxprRV -> [UxprRV] -> (UxprRV -> a) -> Uxec a
  UxArr  :: [UxprRV] -> (UxprRV -> a) -> Uxec a
  deriving Functor

$(makeFree ''Uxec)

type UxecM = Free Uxec

absWithImplicitParameters :: Eq a => Expr (Either Int a) -> Expr (Either Int a)
absWithImplicitParameters = EVal . VAbs . abstract (either Just (const Nothing))

absN :: Expr' -> Expr'
absN = absWithImplicitParameters

absWithExplicitParameters :: Eq a => [a] -> Expr (Either Int a) -> Expr (Either Int a)
absWithExplicitParameters ps = EVal . VAbs . abstract (either (const Nothing) f)
  where f x = (+ 1) <$> elemIndex x ps

absP :: [String] -> Expr' -> Expr'
absP ps = absWithExplicitParameters (map ('@':) ps)

uAbsN :: Ann -> UxprRV -> UxprRV
uAbsN ann = uAbs ann . abstract (either Just (const Nothing))

uAbsP :: Ann -> [String] -> UxprRV -> UxprRV
uAbsP ann ps = uAbs ann . abstract (either (const Nothing) f)
  where f x = (+ 1) <$> elemIndex x ps
