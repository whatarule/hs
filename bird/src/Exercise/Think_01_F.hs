
module Exercise.Think_01_F where

import Prelude hiding (
    Word
  )

import Data.List (
    sort
  )


-- anagrams :: Int -> [Word] -> String

-- |
-- >>> length "aaa"
-- 3
-- >>> (==)(length "aaa") 3
-- True
--


-- / 1.3
-- |

-- commonWords :: Int -> ([Char] -> [Char])

type Text = [Char]
type Word = [Char]

-- words :: [Char] -> [[Char]]

-- toLower :: Char -> Char
-- map :: (a -> b) -> [a] -> [b]
-- map toLower :: [Char] -> [Char]
-- map toLower :: Text -> Text
-- words . map toLower :: Text -> [Word]

sortWords :: [Word] -> [Word]
-- |
-- >>> sortWords ["to","be","or","not","to","be"]
-- ["be","be","not","or","to","to"]
--
sortWords = sort
-- |

countRuns :: [Word] -> [(Int,Word)]
-- |
-- >>> countRuns ["be","be","not","or","to","to"]
-- [(2,"be"),(1,"not"),(1,"or"),(2,"to")]
--
countRuns xs = map f . nubBy (==) $ xs
  where
    f = \x ->
      (g x, x)
    g = \x ->
      length . filter ((==) x) $ xs

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []     =  []
nubBy eq (x:xs) =
  (:) x . nubBy eq . filter (not . eq x) $ xs
-- |


