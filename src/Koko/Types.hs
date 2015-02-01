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

import Bound (Scope, abstract, (>>>=), fromScope, toScope)

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Prompt

import Prelude.Extras (Eq1, Ord1, Show1, Read1)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.State (StateT)
import Control.Monad (ap)
import Data.List (elemIndex)

import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim (Parsec)

type Token    = String
type Token'   = (SourcePos, Token)
type Stream   = [Token']
type Parser a = Parsec Stream () a

data Uxpr f s v =
    EVar v
  | EApp f [f]
  | EAbs s
  | ESeq [f]
  | ESym String
  | EFun String
  | ENil
  | EArr [f]
  deriving (Eq, Ord, Show, Read, Functor)

eVar :: Ann -> v -> UxprR v
eVar ann = U ann . EVar

eApp :: Ann -> UxprR v -> [UxprR v] -> UxprR v
eApp ann u us = U ann (EApp u us)

eAbs :: Ann -> Scope Int UxprR v -> UxprR v
eAbs ann = U ann . EAbs

eSeq :: Ann -> [UxprR v] -> UxprR v
eSeq ann = U ann . ESeq

eSym :: Ann -> String -> UxprR v
eSym ann = U ann . ESym

eFun :: Ann -> String -> UxprR v
eFun ann = U ann . EFun

eNil :: Ann -> UxprR v
eNil ann = U ann ENil

eArr :: Ann -> [UxprR v] -> UxprR v
eArr ann = U ann . EArr

newtype Ann = Ann (Maybe Int)
  deriving (Eq, Ord, Show, Read)

noAnn :: Ann
noAnn = Ann Nothing

data UxprR' a v = U a (Uxpr (UxprR' a v) (Scope Int (UxprR' a) v) v)
type UxprR = UxprR' Ann
type UxprRV = UxprR Variable

mapAnns :: (Ann -> Ann) -> UxprR v -> UxprR v
mapAnns f (U a expr) = U (f a) u'
  where
    u' = case expr of
           EVar v -> EVar v
           EApp u us -> EApp (mapAnns f u) (map (mapAnns f) us)
           EAbs s -> EAbs (toScope (mapAnns f (fromScope s)))
           ESeq us -> ESeq (map (mapAnns f) us)
           EArr us -> EArr (map (mapAnns f) us)
           ESym s -> ESym s
           EFun s -> EFun s
           ENil -> ENil

deriving instance Eq v => Eq (UxprR v)
deriving instance Ord v => Ord (UxprR v)
deriving instance Show v => Show (UxprR v)
deriving instance Read v => Read (UxprR v)

instance Functor UxprR where
  fmap f (U ann uxpr) =
    case uxpr of
      EVar v -> U ann (EVar (f v))
      EApp u us -> U ann (EApp (fmap f u) (map (fmap f) us))
      ESeq us -> U ann (ESeq (map (fmap f) us))
      ESym s -> U ann (ESym s)
      EFun s -> U ann (EFun s)
      EAbs s -> U ann (EAbs (fmap f s))
      ENil -> U ann ENil
      EArr us -> U ann (EArr (map (fmap f) us))

instance Monad UxprR where
  return = U (Ann Nothing) . EVar
  U ann e >>= f =
    case e of
      EVar v -> f v
      EApp u us -> U ann (EApp (u >>= f) (map (>>= f) us))
      ESeq us -> U ann (ESeq (map (>>= f) us))
      ESym s -> U ann (ESym s)
      EFun s -> U ann (EFun s)
      EAbs s -> U ann (EAbs (s >>>= f))
      ENil -> U ann ENil
      EArr us -> U ann (EArr (map (>>= f) us))

instance Applicative UxprR where
  pure = U (Ann Nothing) . EVar
  (<*>) = ap

instance Eq1 UxprR
instance Ord1 UxprR
instance Show1 UxprR
instance Read1 UxprR

data Restart a where
  UncaughtProblem :: Ann -> Problem -> Restart UxprRV

type ExecState = Ann

type Evaluator m a = StateT ExecState (EitherT Problem (PromptT Restart m)) a
type PromptResult m = m (Either Problem UxprRV)

type Variable = Either Int String

data Problem = NonexistentImplicitArgument Int
             | NonexistentFreeVariable String
             | Nonapplicable UxprRV
             | UnknownError
  deriving (Eq, Ord, Show)

data Action a where
  DoPrint :: [UxprRV] -> a -> Action a
  DoPrompt :: Ann -> Problem -> (UxprRV -> a) -> Action a
  deriving Functor

$(makeFree ''Action)

type ActionM = Free Action

type Evaluator' = Evaluator ActionM UxprRV

data Exec a where
  XAnn  :: Ann -> (UxprRV -> a) -> Exec a
  XHalt :: Problem -> Exec a
  XNil  :: (UxprRV -> a) -> Exec a
  XName :: String -> (UxprRV -> a) -> Exec a
  XIdx  :: Int -> (UxprRV -> a) -> Exec a
  XSym  :: String -> (UxprRV -> a) -> Exec a
  XAbs  :: Scope Int UxprR Variable -> (UxprRV -> a) -> Exec a
  XApp  :: UxprRV -> [UxprRV] -> (UxprRV -> a) -> Exec a
  XArr  :: [UxprRV] -> (UxprRV -> a) -> Exec a
  deriving Functor

$(makeFree ''Exec)

type ExecM = Free Exec

absN :: Ann -> UxprRV -> UxprRV
absN ann = eAbs ann . abstract (either Just (const Nothing))

absP :: Ann -> [String] -> UxprRV -> UxprRV
absP ann ps = eAbs ann . abstract (either (const Nothing) f)
  where f x = (+ 1) <$> elemIndex x (map ('@':) ps)
