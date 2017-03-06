import Control.Arrow
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s


parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Functor Parser where
  fmap g p = Parser $ fmap (g *** id) . apply p

instance Applicative Parser where
  pure x = Parser (\s -> pure (x, s))
  Parser f <*> Parser x = Parser (\s -> [(f' x', s'')
                                        | (f', s') <- f s
                                        , (x', s'') <- x s'])

instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  p >>= q = Parser (\s -> [(y, s'') | (x, s') <- apply p s
                                    , (y, s'') <- apply (q x) s'])


