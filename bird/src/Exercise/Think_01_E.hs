
module Exercise.Think_01_E where

-- |
-- >>> (2 ^ 1)^2
-- 4
-- >>>  2 ^(1^2)
-- 2
--

-- |
-- >>> let e = 0
-- >>> let x = 1
-- >>> (==)(x + e) x
-- True
-- >>> (==)(e + x) x
-- True
--
-- >>> let e = []
-- >>> let ls = [0,1]
-- >>> (==)(ls ++ e) ls
-- True
-- >>> (==)(e ++ ls) ls
-- True
--
-- >>> let e = id
-- >>> let f = (+) 1
-- >>> (==)(f . e $ x)(f x)
-- True
-- >>> (==)(e . f $ x)(f x)
-- True
--

