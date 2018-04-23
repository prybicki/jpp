{-# LANGUAGE DeriveFunctor #-}

import Data.Char
import Data.Maybe

import Control.Monad
instance Applicative Parser where
  pure = return
  (<*>) = ap


data Parser a = Parser { runParser :: String -> [(a, String)] } deriving Functor

parse :: Parser a -> String -> [(a, String)]
parse = runParser

parser :: (String -> [(a, String)]) -> Parser a
parser = Parser

empty :: Parser a
empty = parser $ const []

instance Monad Parser where
  return x = Parser $ \s -> [(x, s)]
  m >>= k = Parser $ \s -> [(b, s'') | (a, s') <- runParser m s, (b, s'') <- runParser (k a) s']


-- i) Write a parser item :: Parser Char that reads the first character in the input.

item :: Parser Char
item = parser f where
  f "" = []
  f (c:cs) = [(c,cs)]

-- parse item "bonjour"

-- ii) Write a parser sat :: (Char -> Bool) -> Parser Char that reads and returns the first character in the input only if it satisfies a given predicate.

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  c <- item
  if pred c then return c else empty

-- parse(sat(\b -> b == 'c')) "czesc"


-- iii) Write a parser space :: Parser () that reads a single space.

space :: Parser ()
-- space = sat(\b -> b == ' ') >>= \_ -> return ()
space = do
  sat(\b -> b == ' ')
  return ()


-- iv) Write a parser char :: Char -> Parser () that reads a given input character.

char :: Char -> Parser ()

-- char c = sat (\x -> x == c) >>= return ()
char c = do
  sat(\x -> x == c)
  return ()

-- -- v) Write a parser that reads a given input string.
--
string :: String -> Parser ()
string [] = return ()
string (h:t) = do
  char h
  string t

-- parse (string "kotek") "kotekygtftf"

-- vi) Write a combinator try :: Parser a -> Parser (Maybe a) that tries to apply a given parser. If it succeeds, then the whole parser succeeds, with the same result. If it fails, then the whole parser succeeds returning nothing and without changing the input.

try :: Parser a -> Parser (Maybe a)

-- try p = Parser $ \s ->
  -- case parse p s of
    -- [] -> [(Nothing, s)]
    -- rs -> map(\(r, s) -> (Just r,s)) rs

try p = parser f where
  f cs = g $ parse (fmap Just p) cs where
    g [] = [(Nothing, cs)]
    g xs = xs


-- vii) Write a parser spaces :: Parser () that reads as many consecutive spaces as possible.
spaces :: Parser ()
spaces = do
  x <- try space
  case x of
    Just _ -> spaces
    Nothing -> return ()

-- -- viii) Generalise the previous exercise and write a parser combinator many p = do mx <- try p that applies a given parser as many times as possible. many, many1 :: Parser a -> Parser [a]

many :: Parser a -> Parser [a]

many p = try p >>= \x -> case x of
  Nothing -> return []
  Just h -> many p >>= (\x -> return (h:x))

-- many p = do
--   x <- try p
--   case x of
--     Just _ -> p
--     Nothing -> return ()


-- many1 :: Parser a -> Parser [a]
--
--
-- -- ix) Write a combinator token :: Parser a -> Parser a, it applies a given parser and removes any trailing spaces.
--
token :: Parser a -> Parser a
token p = do
  res <- p
  spaces
  return res

-- -- x) Write a parser digit :: Parser Int that reads a digit and returns its numerical value in {0, . . . , 9}, a parser digits :: Parser [Int] that reads a maximal sequence of digits, and a parser number :: Parser Int that reads a sequence of digits and returns its corresponding natural number.
--
digit :: Parser Int
digit = do
  d <- sat isDigit
  return $ read digitToInt d
digits :: Parser [Int]
digits = many digit



-- number :: Parser Int
number = do
  ds <- digits
  return $ foldl(\n d -> n*10+d) 0 ds
--
-- -- xi) Write a parser combinator (<+>) :: Parser a -> Parser a -> Parser a that given two parsers returns all possible parses obtained by applying any of them.
--
(<+>) :: Parser a -> Parser a -> Parser a
