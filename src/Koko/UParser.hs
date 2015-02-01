{-# LANGUAGE LambdaCase #-}
module Koko.UParser where

import Koko.Types

import Control.Applicative
import Control.Monad (when)
import Text.Parsec.Combinator (choice, eof, sepBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (newPos)
import Text.Parsec.Prim (Parsec, token, try)

import qualified Text.Parsec.Prim as P

tokenThat :: (Token -> Bool) -> Parser Token
tokenThat p = token (show . snd) fst p'
  where p' (_, t) = if p t then Just t else Nothing

parse :: [Token] -> Either ParseError UxprRV
parse xs = P.parse (expr <* eof) "" xs'
  where xs' = [(newPos "" 1 i, x) | (x, i) <- zip xs [1..]]

word :: Token -> Parser ()
word c = skip (tokenThat (== c))

skip :: Parser a -> Parser ()
skip = (>> return ())

expr :: Parsec [Token'] () UxprRV
expr = expr1 `sepBy1` word "," >>= \case
         [e] -> return e
         es  -> uSeq <$> ann <*> pure es
         
  where
    ann         = pure ()
    expr1       = choice [simple, let', application, try explicit, implicit]
    let'        = do try (word "[" *> word "let")
                     mkLet <$> ann <*> many binding <* word ":" <*> bodyUntil "]"
    binding     = ((,) <$> symbolToken <*> expr1)
    application = try (word "[" *> (uApp <$> ann <*> expr1 <*> many expr1) <* word "]")
    explicit    = uAbsP <$> ann <*> (word "{" *> many symbolToken <* word ":")
                        <*> bodyUntil "}"
    implicit    = uAbsN <$> ann <*> (word "{" >> bodyUntil "}")
    bodyUntil t = (word t *> (uNil <$> ann)) <|> (expr <* word t)
    simple      = choice [variable, symbol, index]
    variable    = uVar <$> ann <*> (Right <$> tokenThat ((== '@') . head))
    symbol      = uSym <$> ann <*> symbolToken
    symbolToken = tokenThat (not . (`elem` "%@[]{}:") . head)
    index       = (tokenThat (== "%") *> (uVar <$> ann <*> pure (Left 1)))
                    <|> numberedIndex

mkLet :: Ann -> [(String, UxprRV)] -> UxprRV -> UxprRV
mkLet ann ps e = uApp ann (uAbsP ann (map fst ps) e) (map snd ps)

numberedIndex :: Parser UxprRV
numberedIndex = do
  (_:s) <- tokenThat ((== '%') . head)
  let [(x, [])] = reads s
  when (x <= 0) (fail "Variable index must be positive")
  return (uVar () (Left x))
