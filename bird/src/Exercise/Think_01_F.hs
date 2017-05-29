
module Exercise.Think_01_F where

import Prelude hiding (
    Word
  , take
  , concat
  )

import Data.Char (
    toLower
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
-- >>> putStr . commonWords 3 $ "to be or not to be"
--   be  2
--   to  2
--   not  1
--

commonWords :: Int -> ([Char] -> [Char])
-- |
-- >>> commonWords 3 "to be or not to be"
-- "  be  2\n  to  2\n  not  1\n"
--
commonWords n =
    concat. map showRun . take n
  . sortRuns . countRuns
  . sortWords . words . map toLower
-- |


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

sort :: Ord a => [a] -> [a]
sort []     = []
sort (x:xs) =
  sort lessOrEqual ++ [x] ++ sort greater
  where
    lessOrEqual = [a | a <- xs, a < x || a == x]
    greater     = [a | a <- xs, a > x]


countRuns :: [Word] -> [(Int,Word)]
-- |
-- >>> countRuns ["be","be","not","or","to","to"]
-- [(2,"be"),(1,"not"),(1,"or"),(2,"to")]
--
countRuns wrdLs = map f . nubBy (==) $ wrdLs
  where
    f = \x -> (g x, x)
    g = \x -> length . filter ((==) x) $ wrdLs
-- |

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []     =  []
nubBy eq (x:xs) =
  (:) x . nubBy eq . filter (not . eq x) $ xs


sortRuns :: [(Int,Word)] -> [(Int,Word)]
-- |
-- >>> sortRuns [(2,"be"),(1,"not"),(1,"or"),(2,"to")]
-- [(2,"be"),(2,"to"),(1,"not"),(1,"or")]
--
sortRuns []     = []
sortRuns (x:xs) =
  sortRuns greater ++ [x] ++ sortRuns lessOrEqual
  where
    greater     = [(int,wrd) | (int,wrd) <- xs, int > fst x]
    lessOrEqual = [(int,wrd) | (int,wrd) <- xs, int < fst x || int == fst x]


take :: Int -> [a] -> [a]
take n xs | 0 < n     = unsafeTake n xs
          | otherwise = []

unsafeTake :: Int -> [a] -> [a]
unsafeTake _ []     = []
unsafeTake 1 (x: _) = [x]
unsafeTake n (x:xs) = x : unsafeTake (n - 1) xs


showRun :: (Int,Word) -> String
-- |
-- >>> showRun (2,"be")
-- "  be  2\n"
--
showRun (int,wrd) =
  "  " ++ wrd ++ "  " ++ show int ++ "\n"
-- |


concat :: [[a]] -> [a]
-- |
-- >>> concat [[0],[1]]
-- [0,1]
--
concat = foldr (++) []
-- |


