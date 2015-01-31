module Koko where

import Koko.Types

import Bound (abstract, instantiate)
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.Either (runEitherT, left)
import Control.Monad.Writer (tell, runWriter)

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
