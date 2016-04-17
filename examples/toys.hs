-----------------------------------------------------------------------------
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
-- A collection of toys
-----------------------------------------------------------------------------

module Toys where

import Data.Sequence (Seq, empty, (|>), ViewL(..), viewl)

import Control.AFSM
import Control.AFSM.Event


-- Prime Numbers

ltest :: [Int] -> Int -> Bool
ltest [] _ = True
ltest (x:xs) a = if mod a x == 0 then False else if x * x > a then True else ltest xs a

-- [2,3,5,...]
-- origin_primes = filter (ltest origin_primes) [2..]
-- The problem is the first prime number should be given by us.
origin_primes = 2:(filter (ltest origin_primes) [3..])

seqtest :: (ViewL Int) -> Int -> Bool
seqtest EmptyL _ = True
seqtest (x :< sq) a = if mod a x == 0 then False else if x * x > a then True else seqtest (viewl sq) a


-- The storage maintains a list of prime numbers it has known so far.
-- When a new number comes, the SM test the number with the list, if it pass the test, then add it into the list, else drop it.
primeSM :: SM (Seq Int) Int (Event Int)
primeSM = simpleSM f empty
  where
    f sq a = 
      if seqtest (viewl sq) a then 
        (sq |> a, Event a)
      else
        (sq, NoEvent)

-- [2,3,5,...]      
primes = extractEvents $ smfmap (primeSM) [2..]

--

