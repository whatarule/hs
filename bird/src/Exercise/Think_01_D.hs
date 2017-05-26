
module Exercise.Think_01_D where

import Data.Char (
    toLower
  )

-- |
-- >>> let text = "AAA\n BBB"
-- >>> words text
-- ["AAA","BBB"]
-- >>> words . map toLower $ text
-- ["aaa","bbb"]
-- >>> map ( map toLower ) . words $ text
-- ["aaa","bbb"]
--
-- >>> (==)(words . map toLower $ text)(map ( map toLower ) . words $ text)
-- True
--

