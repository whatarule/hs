
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


-- // instance

instance Functor Parser where
--  fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P $ \inp -> case parse p inp of
      []        -> []
      [(v,out)] -> [(g v,out)]
-- |
-- >>> flip parse "" . fmap (+1) . return $ 0
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
-- >>> flip parse "" . pure $ 1
-- [(1,"")]
-- >>> flip parse "" $ pure (+1) <*> pure 1
-- [(2,"")]
--

instance Monad Parser where
--  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \inp -> case parse p inp of
      [] -> []
      [(v,out)] -> parse (f v) out
-- |
-- >>> flip parse "" $ pure 0 >>= (\_ -> pure 1)
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
-- >>> flip parse "" $ empty
-- []
-- >>> flip parse "" $ pure 1 <|> empty
-- [(1,"")]
--



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

sat :: (Char -> Bool) -> Parser Char
sat p = P $ \inp -> case flip parse inp $ p <$> item of
  [(True,_)] -> flip parse inp item
  _          -> empty


-- // String

string :: String -> Parser String
-- |
-- >>> flip parse "aabbcc" $ string "aa"
-- [("aa","bbcc")]
-- >>> flip parse "aabbcc" $ string "bb"
-- []
-- >>> flip parse "aabbcc" $ string "aaa"
-- []
--
string ""     = pure ""
string (x:xs) = P $ \inp ->
  case flip parse inp $ char x of
    [(y,ys)] -> flip parse ys $ (y:) <$> string xs
    _        -> empty
-- |


-- // Char

char :: Char -> Parser Char
-- |
-- >>> flip parse "abc" $ char 'a'
-- [('a',"bc")]
char x = sat (== x)
-- |

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


-- // Applicative

many :: Parser a -> Parser [a]
-- |
-- >>> flip parse "123abc" $ many digit
-- [("123","abc")]
-- >>> flip parse "abc" $ many digit
-- [("","abc")]
-- >>> flip parse "abc" $ many1 digit
-- []
-- >>> flip parse "abc" $ some digit
-- []
--
many p = many1 p <|> pure []
many1 :: Parser a -> Parser [a]
many1 p = P $ \inp ->
  case flip parse inp p of
    [(x,xs)] -> flip parse xs $ (x:) <$> many p
    _        -> empty
some :: Parser a -> Parser [a]
some = many1
-- |

ident :: Parser String
-- |
-- >>> flip parse "a01!" $ ident
-- [("a01","!")]
--
ident = P . parse $
--P $ \inp ->
--case flip parse inp $ lower of
--  [(x,xs)] -> flip parse xs $ (x:) <$> many alphanum
--  _        -> empty
  (:) <$> lower <*> many alphanum
-- |

nat :: Parser Int
-- |
-- >>> flip parse "001a" $ nat
-- [(1,"a")]
--
nat = P $ \inp ->
  flip parse inp $ read <$> some digit
-- |


-- // space

space :: Parser ()
-- |
-- >>> flip parse "   aaa" $ space
-- [((),"aaa")]
--
space = P $ \inp ->
  flip parse inp $ pure () <$> many (sat isSpace)
-- |

token :: Parser a -> Parser a
-- |
-- >>> flip parse " ( aaa ) " $ token (char '(')
-- [('(',"aaa ) ")]
--
token p = P $ \inp ->
  flip parse inp $ space *> p <* space
-- |


