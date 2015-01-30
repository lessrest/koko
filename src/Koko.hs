module Koko where

import Control.Applicative

import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Prim as P

data Expr = EVar String
          | EApp Expr [Expr]
          | ESym String
          deriving (Eq, Show)

type Token = String
type Token' = (P.SourcePos, Token)
type Stream = [Token']
type Parser a = P.Parsec Stream () a

tokenThat :: (Token -> Bool) -> Parser Token
tokenThat p = P.token show fst p'
  where p' (_, t) = if p t then Just t else Nothing

parse :: [Token] -> Either P.ParseError Expr
parse xs = P.parse expr "" xs'
  where xs' = [(P.newPos "" 1 i, x) | (x, i) <- zip xs [1..]]

word :: Token -> Parser ()
word c = tokenThat (== c) >> return ()

expr :: P.Parsec [Token'] () Expr
expr = P.choice [application, simple]
  where
    application = P.try (word "[" *> (EApp <$> expr <*> P.many expr) <* word "]")
    simple      = P.choice [variable, symbol]
    variable    = EVar <$> tokenThat ((== '@') . head)
    symbol      = ESym <$> tokenThat (not . (`elem` "@[]") . head)

