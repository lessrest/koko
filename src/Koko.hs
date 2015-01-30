module Koko where

import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Prim as P

data Expr = EVar String
          deriving (Eq, Show)

type Token = String
type Token' = (P.SourcePos, Token)
type Stream = [Token']
type Parser a = P.Parsec Stream () a

tokenThat :: (Token -> Bool) -> Parser Token
tokenThat p = P.token (show . snd) fst p'
  where p' (_, t) = if p t then Just t else Nothing

class Result r where
  fromEither :: Either P.ParseError e -> r e

instance Result Maybe where
  fromEither = either (const Nothing) Just

parse :: Result r => [Token] -> r Expr
parse xs = fromEither $ P.parse expr "" xs'
  where xs' = ([(P.newPos "" 1 i, x) | x <- xs, i <- [1..]])

expr :: P.Parsec [Token'] () Expr
expr = tokenThat (const True) >> return (EVar "@foo")
