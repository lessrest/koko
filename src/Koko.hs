 {-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Koko where

import Bound (Scope, (>>>=), abstract, instantiate)
import Control.Applicative
import Control.Monad (ap, when)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Prelude.Extras (Eq1, Ord1, Show1, Read1)

import Text.Parsec.Combinator (choice, eof)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos, newPos)
import Text.Parsec.Prim (Parsec, token, try)

import qualified Text.Parsec.Prim as P

type Token = String
type Token' = (SourcePos, Token)
type Stream = [Token']
type Parser a = Parsec Stream () a
type Output = [String]
type Evaluator a = EitherT String (Writer [String]) a

tokenThat :: (Token -> Bool) -> Parser Token
tokenThat p = token (show . snd) fst p'
  where p' (_, t) = if p t then Just t else Nothing

parse :: [Token] -> Either ParseError Expr'
parse xs = P.parse (expr <* eof) "" xs'
  where xs' = [(newPos "" 1 i, x) | (x, i) <- zip xs [1..]]

word :: Token -> Parser ()
word c = skip (tokenThat (== c))

skip :: Parser a -> Parser ()
skip = (>> return ())

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

type Expr' = Expr (Either Int String)
type Value' = Value (Either Int String)

absWithImplicitParameters :: Eq a => Expr (Either Int a) -> Expr (Either Int a)
absWithImplicitParameters = EVal . VAbs . abstract (either Just (const Nothing))

absN :: Expr' -> Expr'
absN = absWithImplicitParameters

expr :: Parsec [Token'] () Expr'
expr = choice [simple, application, abstraction]
  where
    application = try (word "[" *> (EApp <$> expr <*> many expr) <* word "]")
    abstraction = absWithImplicitParameters <$> (word "{" >> try body)
    body        = (word "}" *> pure (EVal VNil)) <|> (expr <* word "}")
    simple      = choice [variable, symbol, index]
    variable    = (EVar . Right) <$> tokenThat ((== '@') . head)
    symbol      = EVal . VSym <$> tokenThat (not . (`elem` "%@[]{}") . head)
    index       = (tokenThat (== "%") *> pure (EVar (Left 1))) <|> numberedIndex

numberedIndex :: Parser Expr'
numberedIndex = do
  (_:s) <- tokenThat ((== '%') . head)
  let [(x, [])] = reads s
  when (x <= 0) (fail "Variable index must be positive")
  return (EVar (Left x))

evaluate :: Expr' -> (Either String Expr', [String])
evaluate e = runWriter (runEitherT (evaluate' e))

evaluate' :: Expr' -> Evaluator Expr'
evaluate' (EVal (VSym s)) = pure (EVal (VSym s))
evaluate' (EVal (VAbs e)) = pure (EVal (VAbs e))
evaluate' (EApp e xs) = flip apply xs =<< evaluate' e
evaluate' (EVar (Right v)) = pure (EVal (VFun v))
evaluate' _ = left "Evaluation error"

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
  f <- maybe (left "No such function") pure (lookup s functions)
  vs <- mapM evaluate' es
  f vs
apply _ _ = left "Application error"
