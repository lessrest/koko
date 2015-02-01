{-# LANGUAGE LambdaCase #-}
module Koko.Parser where

import Koko.Types

import Control.Applicative
import Control.Monad (when)
import Text.Parsec.Combinator (choice, eof, sepBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (newPos, sourceColumn)
import Text.Parsec.Prim (Parsec, token, try, getPosition)

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

ann :: Parsec a b Ann
ann = Ann . Just . sourceColumn <$> getPosition

expr :: Parsec [Token'] () UxprRV
expr = expr1 `sepBy1` word "," >>= \case
         [e] -> return e
         es  -> eSeq <$> ann <*> pure es
         
  where
    expr1       = choice [simple, let', application, try explicit, implicit]
    let'        = do try (word "[" *> word "let")
                     mkLet <$> ann <*> many binding <* word ":" <*> bodyUntil "]"
    binding     = ((,) <$> symbolToken <*> expr1)
    application = try (word "[" *> (eApp <$> ann <*> expr1 <*> many expr1) <* word "]")
    explicit    = absP <$> ann <*> (word "{" *> many symbolToken <* word ":")
                       <*> bodyUntil "}"
    implicit    = absN <$> ann <*> (word "{" >> bodyUntil "}")
    bodyUntil t = (word t *> (eNil <$> ann)) <|> (expr <* word t)
    simple      = choice [variable, symbol, index]
    variable    = eVar <$> ann <*> (Right <$> tokenThat ((== '@') . head))
    symbol      = eSym <$> ann <*> symbolToken
    symbolToken = tokenThat (not . (`elem` "%@[]{}:") . head)
    index       = (tokenThat (== "%") *> (eVar <$> ann <*> pure (Left 1)))
                    <|> numberedIndex

mkLet :: Ann -> [(String, UxprRV)] -> UxprRV -> UxprRV
mkLet a ps e = eApp a (absP a (map fst ps) e) (map snd ps)

numberedIndex :: Parser UxprRV
numberedIndex = do
  (_:s) <- tokenThat ((== '%') . head)
  let [(x, [])] = reads s
  when (x <= 0) (fail "Variable index must be positive")
  eVar <$> ann <*> pure (Left x)
