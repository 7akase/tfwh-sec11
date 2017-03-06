import Control.Arrow ((***))
import Data.Functor (fmap)
import Control.Applicative (pure, (<*>))
import Control.Monad (return , (>>=))

import Data.Char (isLower, isDigit)
import Prelude hiding (fail)

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

getc :: Parser Char
getc = Parser f
         where
           f [] = []
           f (c:cs) = [(c,cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- getc; if p c then return c else fail }

fail = Parser (\s -> [])

char :: Char -> Parser ()
char x = do { c <- sat (== x); return () }

string :: String -> Parser ()
string [] = return ()
string (x : xs)  = do {char x; string xs; return () }

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do {d <- sat isDigit; return (cvt d)}
          where
            cvt d = fromEnum d - fromEnum '0'


