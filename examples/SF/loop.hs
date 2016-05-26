{-# LANGUAGE Arrows #-}

import Control.Arrow

import Data.AFSM.SF
import Data.Maybe

-- newtype SF a b = SF (a -> (SF a b, b))

sf :: SF (Int,Int) (Int,Int)
sf = arr (\(x, sum) -> (x + sum, x + sum))

delay :: Int -> SF Int Int
delay inital = SF (\x -> (delay x, inital))

sf1 :: SF Int Int
-- sf1 = loop (sf >>> (second (delay 0)))
sf1 = proc x -> do
  rec { (y, next_sum) <- sf -< (x, sum);
        sum <- delay 0 -< next_sum; }
  returnA -< y
  
-- >>> sfmap sf1 [1,2,3,4,5]
-- [1,3,6,10,15]
