
module Parser (
  ) where

import Data.Char

import Prelude hiding (
    return
  )

-- // Type

type Parser a =
  String -> [(a,String)]

-- // Base

return :: a -> Parser a
-- |
return v = \ inp ->
  [(v,inp)]
-- |

failure :: Parser a
-- |
failure = \ inp ->
  []

item :: Parser Char
-- |
item = \ inp -> case inp of
  [] ->
    []
  (x:xs) ->
    [(x, xs)]
-- |

parse :: Parser a -> String -> [(a,String)]
-- |
-- >>> parse (return 1) "abc"
-- [(1,"abc")]
-- >>> parse failure "abc"
-- []
-- >>> parse item ""
-- []
-- >>> parse item "abc"
-- [('a',"bc")]
--
parse p inp = p inp
-- |



