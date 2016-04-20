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

import System.Random
import Data.Tuple

import Data.Sequence (Seq, empty, (|>), ViewL(..), viewl)

import Control.AFSM
import Control.AFSM.Event

----------
-- Fibs --
----------

-- the nice code from experts.
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- make beginners' life easier.
--   the SM stores f_n and f_{n+1},
--   for each time we update the storage to (f_{n+1}, f_n + f_{n+1}),
--   and outputs f_n.
fibsSM :: SM (Int, Int) () Int
fibsSM = simpleSM (\(a, b) () -> ((b, a+b), a)) (0, 1)

ourfibs = simpleSrc fibsSM

-------------------
-- Random Source --
-------------------
randSM :: StdGen -> SM StdGen () Int
randSM = simpleSM (\g () -> swap $ next g)

randSrc :: Int -> [Int]
randSrc = simpleSrc.randSM.mkStdGen

randSamples = take 5 $ randSrc 111

-------------------
-- Prime Numbers --
-------------------

ltest :: [Int] -> Int -> Bool
ltest [] _ = True
ltest (x:xs) a = if mod a x == 0 then False else if x * x > a then True else ltest xs a

-- [2,3,5,...]
-- origin_primes = filter (ltest origin_primes) [2..]
-- The problem is the first prime number should be given by hand.
origin_primes = 2:(filter (ltest origin_primes) [3..])

seqtest :: (ViewL Int) -> Int -> Bool
seqtest EmptyL _ = True
seqtest (x :< sq) a = if mod a x == 0 then False else if x * x > a then True else seqtest (viewl sq) a


-- The storage maintains a list of prime numbers it has known so far.
-- Here we use Seq as the storage, it is similar with list, but has more efficient append operation.
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
