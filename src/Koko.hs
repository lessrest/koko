module Koko where

import Control.Applicative
  ((<$>), (<*>), (<*), (*>),
   pure, many)
  
import Control.Monad.Writer (Writer, tell, runWriter)
import Control.Monad.Reader
import Control.Monad.Trans.Either

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
type Evaluator a = ReaderT [Value] (EitherT String (Writer [String])) a

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
           | VFun String
           | VNil
             deriving (Eq, Show)

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

evaluate :: Expr -> (Either String Value, [String])
evaluate e = runWriter (runEitherT (runReaderT (evaluate' e) []))

evaluate' :: Expr -> Evaluator Value
evaluate' (ESym s) = pure (VSym s)
evaluate' (EAbs e) = pure (VAbs e)
evaluate' (EApp e xs) = flip apply xs =<< evaluate' e
evaluate' (EVar v) = pure (VFun v)
evaluate' (EIdx i) = (!! (i - 1)) <$> ask
evaluate' _ = lift (left "Evaluation error")

functions :: [(String, [Value] -> Evaluator Value)]
functions = [("@print-line", doPrint)]
  where
    doPrint v = tell [unwords (map output v) ++ "\n"] >> pure VNil

output :: Value -> String
output (VSym s) = s
output _ = "<value>"

apply :: Value -> [Expr] -> Evaluator Value
apply (VAbs e) es = do
  vs <- mapM evaluate' es
  local (const vs) (evaluate' e)
apply (VFun s) es = do
  f <- maybe (lift (left "No such function")) pure (lookup s functions)
  vs <- mapM evaluate' es
  f vs
apply _ _ = lift (left "Application error")
