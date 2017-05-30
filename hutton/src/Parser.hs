
module Parser (
  ) where

import Data.Char
import Control.Applicative
  hiding (many, some)

import Prelude hiding (
    return
  )

-- // Type

newtype Parser a =
  P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
-- |
parse (P p) inp = p inp
-- |


-- // Base

return :: a -> Parser a
-- |
-- >>> parse (return 1) "abc"
-- [(1,"abc")]
--
return v = P $ \inp -> [(v,inp)]
-- |

failure :: Parser a
-- |
-- >>> parse failure "abc"
-- []
--
failure = P $ \inp -> []

item :: Parser Char
-- |
-- >>> parse item ""
-- []
-- >>> parse item "abc"
-- [('a',"bc")]
--
item = P $ \inp -> case inp of
  []     -> []
  (x:xs) -> [(x, xs)]
-- |

bool :: (a -> Bool) -> Parser a -> Parser a
bool f p = P $ \inp -> case parse (f <$> p) inp of
  [(True,_)] -> parse p inp
  _          -> empty


-- // instance

instance Functor Parser where
--  fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P $ \inp -> case parse p inp of
      []        -> []
      [(v,out)] -> [(g v,out)]
-- |
-- >>> parse (fmap (+1) . return $ 0) ""
-- [(1,"")]
--

instance Applicative Parser where
--  pure :: a -> Parser a
    pure v = P $ \inp -> [(v,inp)]
--  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P $ \inp -> case parse pg inp of
      []        -> []
      [(g,out)] -> parse (fmap g px) out
-- |
-- >>> parse (pure 1) ""
-- [(1,"")]
-- >>> parse (pure (+1) <*> pure 1) ""
-- [(2,"")]
--

instance Monad Parser where
--  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \inp -> case parse p inp of
      []        -> []
      [(v,out)] -> parse (f v) out
-- |
-- >>> parse (pure 0 >>= (\_ -> pure 1)) ""
-- [(1,"")]
--

instance Alternative Parser where
--  empty :: Parser a
    empty = P $ \inp -> []
--  (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \inp -> case parse p inp of
      []        -> parse q inp
      [(v,out)] -> [(v,out)]
-- |
-- >>> parse empty ""
-- []
-- >>> parse (pure 1 <|> empty) ""
-- [(1,"")]
--


-- // Char

sat :: (Char -> Bool) -> Parser Char
sat p = bool p item

char :: Char -> Parser Char
-- |
-- >>> parse (char 'a') "abc"
-- [('a',"bc")]
char x = sat (== x)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum


-- // String

string :: String -> Parser String
-- |
-- >>> parse (string "aa") "aabbcc"
-- [("aa","bbcc")]
-- >>> parse (string "bb") "aabbcc"
-- []
-- >>> parse (string "aaa") "aabbcc"
-- []
string ""     = pure ""
string (x:xs) = (:) <$> char x <*> string xs


-- // Applicative

many :: Parser a -> Parser [a]
-- |
-- >>> parse (many digit) "123abc"
-- [("123","abc")]
-- >>> parse (many digit) "abc"
-- [("","abc")]
-- >>> parse (many1 digit) "abc"
-- []
-- >>> parse (some digit) "abc"
-- []
many p = many1 p <|> pure []
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p
some :: Parser a -> Parser [a]
some = many1
-- |

ident :: Parser String
-- |
-- >>> parse ident "a01!"
-- [("a01","!")]
ident = (:) <$> lower <*> many alphanum

nat :: Parser Int
-- |
-- >>> parse nat "001a"
-- [(1,"a")]
nat = read <$> some digit

int :: Parser Int
-- |
-- >>> parse int "-1a"
-- [(-1,"a")]
-- >>> parse int "1a"
-- [(1,"a")]
int = (*(-1)) <$> (char '-' *> nat) <|> nat


-- // space

space :: Parser ()
-- |
-- >>> parse space "   aaa "
-- [((),"aaa ")]
space = pure () <$> many (sat isSpace)

token :: Parser a -> Parser a
-- |
-- >>> parse (token $ char '(') $ "  ( aaa) "
-- [('(',"aaa) ")]
token p = space *> p <* space

identifier :: Parser String
-- |
-- >>> parse identifier "  a01 ! "
-- [("a01","! ")]
identifier = token ident

natural :: Parser Int
-- |
-- >>> parse natural "  001 a "
-- [(1,"a ")]
natural = token nat

integer :: Parser Int
-- |
-- >>> parse integer "  -1 a "
-- [(-1,"a ")]
-- >>> parse integer "  1 a "
-- [(1,"a ")]
integer = token int

symbol :: String -> Parser String
-- |
-- >>> parse (symbol "aa") "  aa bbcc"
-- [("aa","bbcc")]
-- >>> parse (symbol "bb") "aa  bb cc"
-- []
-- >>> parse (symbol "aaa") "  aa bbcc"
-- []
symbol xs = token (string xs)


p :: Parser [Int]
-- |
-- >>> parse p "  [ 1, 2, 3 ] "
-- [([1,2,3],"")]
-- >>> parse p "  [ 1, 2,  ] "
-- []
p = (:) <$> (symbol "[" *> natural)
        <*> many (symbol "," *> natural)
        <* symbol "]"


-- // bnf
-- |
-- >>> parse expr "2*3+4"
-- [(10,"")]
-- >>> parse expr "2*(3+4)"
-- [(14,"")]
-- >>> parse expr "2+3+4"
-- [(9,"")]
expr :: Parser Int
expr = (+) <$> term <*> (symbol "+" *> expr) <|> term
term :: Parser Int
term = (*) <$> factor <*> (symbol "*" *> term) <|> factor
factor :: Parser Int
factor = (symbol "(" *> expr) <* symbol ")" <|> natural

eval :: String -> Int
-- |
-- >>> eval "2*3+4"
-- 10
-- >>> eval "2*(3+4)"
-- 14
-- >>> eval "2 * (3 + 4)"
-- 14
eval xs = case parse expr xs of
  [(n,[])]  -> n
  [(_,out)] -> error $ "unused input " ++ out
  []        -> error $ "invalid input"
-- >>> eval "-1"
-- *** Exception: invalid input
-- >>> eval "2 * (3 - 4)"
-- *** Exception: unused input * (3 - 4)

