module Koko.Parser where

import Koko.Types

import Control.Applicative
import Control.Monad (when)
import Text.Parsec.Combinator (choice, eof)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (newPos)
import Text.Parsec.Prim (Parsec, token, try)

import qualified Text.Parsec.Prim as P

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

expr :: Parsec [Token'] () Expr'
expr = choice [simple, application, try explicit, implicit]
  where
    application = try (word "[" *> (EApp <$> expr <*> many expr) <* word "]")
    explicit    = absP <$> (word "{" *> many symbolToken <* word ":")
                       <*> body
    implicit    = absN <$> (word "{" >> body)
    body        = (word "}" *> pure (EVal VNil)) <|> (expr <* word "}")
    simple      = choice [variable, symbol, index]
    variable    = (EVar . Right) <$> tokenThat ((== '@') . head)
    symbol      = EVal . VSym <$> symbolToken
    symbolToken = tokenThat (not . (`elem` "%@[]{}:") . head)
    index       = (tokenThat (== "%") *> pure (EVar (Left 1))) <|> numberedIndex

numberedIndex :: Parser Expr'
numberedIndex = do
  (_:s) <- tokenThat ((== '%') . head)
  let [(x, [])] = reads s
  when (x <= 0) (fail "Variable index must be positive")
  return (EVar (Left x))
