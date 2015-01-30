module Koko where

import Control.Applicative
  ((<$>), (<*>), (<*), (*>),
   many)

import Text.Parsec.Combinator (choice)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos, newPos)
import Text.Parsec.Prim (Parsec, token, try)

import qualified Text.Parsec.Prim as P

data Expr = EVar String
          | EApp Expr [Expr]
          | ESym String
          deriving (Eq, Show)

type Token = String
type Token' = (SourcePos, Token)
type Stream = [Token']
type Parser a = Parsec Stream () a

tokenThat :: (Token -> Bool) -> Parser Token
tokenThat p = token show fst p'
  where p' (_, t) = if p t then Just t else Nothing

parse :: [Token] -> Either ParseError Expr
parse xs = P.parse expr "" xs'
  where xs' = [(newPos "" 1 i, x) | (x, i) <- zip xs [1..]]

word :: Token -> Parser ()
word c = tokenThat (== c) >> return ()

expr :: Parsec [Token'] () Expr
expr = choice [application, simple]
  where
    application = try (word "[" *> (EApp <$> expr <*> many expr) <* word "]")
    simple      = choice [variable, symbol]
    variable    = EVar <$> tokenThat ((== '@') . head)
    symbol      = ESym <$> tokenThat (not . (`elem` "@[]") . head)

