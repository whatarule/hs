
-- {-# LANGUAGE FlexibleContexts #-}

module PointFree where

import Control.Monad (join)

-- >>> :set -XFlexibleContexts
-- |
-- >>> const 1 2
-- 1
-- >>> const (id 1) $ 2
-- 1
-- >>> (const . id $ 1) 2
-- 1
-- >>> ((.) const id) 1 $ 2
-- 1
-- >>> ((const .) id) 1 $ 2
-- 1
--
-- >>> ((id .) id) 1
-- 1
-- >>> ((.) (id .) id) id $ 1
-- 1
-- >>> ((id .) .  id) id $ 1
-- 1
--
-- >>> (((.) (const .) id) id ) 1 2
-- 1
-- >>> ((const .) . id $ id) 1 "a"
-- 1
-- >>> ((const .) . id) (id) 1 "a"
-- 1
-- >>> ((const .) . id) (+1) 1 "a"
-- 2
--
-- >>> (((+) .) . id) (+1) 1 2
-- 4
-- >>> (((.) (+)) . id) (+1) 1 2
-- 4
-- >>> ((.) ((.) (+)) id) (+1) 1 2
-- 4
-- >>> ((.).(.) (+) $ id) (+1) 1 2
-- 4
-- >>> ((.)(.)(.) (+) $ id) (+1) 1 2
-- 4
-- >>> ((.)(.)(.) (+)) (id) (+1) 1 2
-- 4
-- >>> ((.)(.)(.)) (+) (id) (+1) 1 2
-- 4
-- >>> (.)(.)(.) (+) (id) (+1) 1 2
-- 4
-- >>> ((.).(.) (+)) (id) (+1) 1 2
-- 4
-- >>> ((.).(.) (+)) (*2) (+1) 1 2
-- 6
-- >>> ((.).(.) (+)) (*2) (+1) 1 2
-- 6
--
-- >>> ((+) . (*2)) 1 2
-- 4
-- >>> ((.)(+)(*2)) 1 2
-- 4
-- >>> (.)(+)(*2) 1 2
-- 4
--
-- >>> ((+) . (*) 2) 2 1
-- 5
-- >>> ((+) .) ((*) 2) 2 1
-- 5
-- >>> ((.)(+)) ((*) 2) 2 1
-- 5
-- >>> (.)(+)(*2) 2 1
-- 5
--
-- >>> (.)(id)(id) 1
-- 1
-- >>> (.)(id)(+) 1 $ 2
-- 3
-- >>> ((.)(id)(+) 1) 2
-- 3
-- >>> (.)(+)(id) 1 2
-- 3
-- >>> (.)(*)(id) 1 2
-- 2
-- >>> (.)(*)(+1) 1 2
-- 4
-- >>> (.)(*2)(+1) 1
-- 4
--
-- >>> ((.)(*2).(+)) 2 1
-- 6
-- >>> (.)((*2) .) (+) 2 1
-- 6
-- >>> (.)((.)(*2)) (+) 2 1
-- 6
-- >>> ((.)(*2).(+) $ 2) 1
-- 6
--
-- >>> ((.)(*).(+)) 2 1 2
-- 6
-- >>> ((.)(*).(+)) 2 1 3
-- 9
-- >>> ((.)((.)(*)) (+)) 2 1 3
-- 9
-- >>> ((.)((*).) (+)) 2 1 3
-- 9
-- >>> (((*).).(+)) 2 1 3
-- 9
-- >>> ((.)((*).)(+)) 2 1 3
-- 9
-- >>> (.)((*).)(+) 2 1 3
-- 9
-- >>> (.)((*).)(+) 1 1 3
-- 6
-- >>> (.)((*3).)(+) 1 1
-- 6
-- >>> flipCab ((.)((*).)(+)) 3 1 1
-- 6
-- >>> join (flipCab ((.)((*).)(+)) 3) 1
-- 6
-- >>> (.)(join)(flipCab ((.)((*).)(+))) 3 1
-- 6
-- >>> (.)(join.)(flipCab) ((.)((*).)(+)) 3 1
-- 6
--

flipCab :: (a -> b -> c -> d) -> c -> a -> b -> d
flipCab f c a b = f a b c

-- |
-- >>> (.)(.)(.) (*) (id) (id) 2 1
-- 2
-- >>> (.)(.)(.) (*) (id) (+1) 2 1
-- 3
-- >>> (.)(.)(.) (*) (id) (+1) 2 2
-- 6
-- >>> (.)(.)(.) ((*).) (id) (+) 1 2 2
-- 6
-- >>> (.)(.)(.) ((*).) (id) (+) 2 1 3
-- 9
--
-- >>> join (+) 2
-- 4
-- >>> join (.) (id) 2
-- 2
-- >>> join (.) (+2) 2
-- 6
--

-- >>> ((.).(.) (*2)) (+) 2 1
-- >>> ((.).(.) (*2) (+)) 2 1
-- >>> ((.)(+)(*)) 2 2 1
--
-- >>> ((+) . (*2) . (+1)) 1 2
-- 6
-- >>> ((+) . (.)(*2)(+1)) 1 2
-- 6
-- >>> ((.)(+) . (.)(*2)) (+1) 1 2
-- 6
-- >>> ((.) ((.)(+)) . (.)) (*2) (+1) 1 2
-- 6
--
-- >>> ((.) id id) 1
-- 1
-- >>> (.) ((.) id id) id $ 1
-- 1
-- >>> ((.) id . ((.) id id) $ id) 1
-- 1
--

-- >>> (.(+1)) . ((.).(.) (+) $ id) (id) (id) (id) 1 $ 2
--


