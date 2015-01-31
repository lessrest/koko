module Koko where

import Control.Applicative
  ((<$>), (<*>), (<*), (*>),
   pure, many)
import Control.Monad (when)
import Data.Monoid

import Text.Parsec.Combinator (choice, eof)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos, newPos)
import Text.Parsec.Prim (Parsec, token, try, (<|>))

import qualified Text.Parsec.Prim as P

type Token = String
type Token' = (SourcePos, Token)
type Stream = [Token']
type Parser a = Parsec Stream () a
type Output = [String]

tokenThat :: (Token -> Bool) -> Parser Token
tokenThat p = token (show . snd) fst p'
  where p' (_, t) = if p t then Just t else Nothing

parse :: [Token] -> Either ParseError Expr
parse xs = P.parse (expr <* eof) "" xs'
  where xs' = [(newPos "" 1 i, x) | (x, i) <- zip xs [1..]]

word :: Token -> Parser ()
word c = skip (tokenThat (== c))

skip :: Parser a -> Parser ()
skip = (>> return ())

data Expr = EVar String
          | EApp Expr [Expr]
          | ESym String
          | EIdx Int
          | EAbs Expr
          | ENil
          deriving (Eq, Show)

data Value = VSym String
           | VAbs Expr
           | VNil
             deriving (Eq, Show)

type State = (Value, Output)

instance Monoid Value where
  mempty = VNil
  mappend _ b = b

expr :: Parsec [Token'] () Expr
expr = choice [simple, application, abstraction]
  where
    application = try (word "[" *> (EApp <$> expr <*> many expr) <* word "]")
    abstraction = EAbs <$> (word "{" >> try body)
    body        = (word "}" *> pure ENil) <|> (expr <* word "}")
    simple      = choice [variable, symbol, index]
    variable    = EVar <$> tokenThat ((== '@') . head)
    symbol      = ESym <$> tokenThat (not . (`elem` "%@[]{}") . head)
    index       = (tokenThat (== "%") *> pure (EIdx 1)) <|> numberedIndex

numberedIndex :: Parser Expr
numberedIndex = do
  (_:s) <- tokenThat ((== '%') . head)
  let [(x, [])] = reads s
  when (x <= 0) (fail "Variable index must be positive")
  return (EIdx x)

value :: Value -> Either String State
value v = pure (v, [])

evaluate :: Expr -> Either String State
evaluate (ESym s) = value (VSym s)
evaluate (EAbs e) = value (VAbs e)
evaluate (EApp e []) = apply =<< evaluate e
evaluate _ = Left "Evaluation error"

apply :: State -> Either String State
apply (VAbs e, _) = evaluate e
apply _ = Left "Application error"
