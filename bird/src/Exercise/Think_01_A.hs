
module Exercise.Think_01_A where

import Data.List (
    sort
  , concat
  )

double :: Integer -> Integer
-- |
-- >>> map double [1,4,4,3]
-- [2,8,8,6]
-- >>> map (double . double) [1,4,4,3]
-- [4,16,16,12]
-- >>> map double []
-- []
--
double x = 2*x
-- |


-- |
-- >>> let ls = [0,1,2,3]
-- >>> (==)(sum . map double $ ls)(double . sum $ ls)
-- True

-- |
-- >>> let ls = [[0,1],[2,3]]
-- >>> map sum $ ls
-- [1,5]
-- >>> concat $ ls
-- [0,1,2,3]
-- >>> (==)(sum . map sum $ ls)(sum . concat $ ls)
-- True

-- |
-- >>> let ls = [0,1,2,3]
-- >>> (==)(sum . sort $ ls)(sum $ ls)
-- True
--

