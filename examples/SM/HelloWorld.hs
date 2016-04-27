-----------------------------------------------------------------------------
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
-- HelloWorld
-----------------------------------------------------------------------------

{-# LANGUAGE PartialTypeSignatures #-}

module HelloWorld where

import Control.AFSM
import Control.AFSM.SMH

import Data.Maybe

-- | x_i donates the ith element of the input, y_i donates the ith element of the output.

-- $setup
-- >>> let test0 = [1,2,3,4]::[Int]


-- basic machines


-- | the sum of the history, y_n = \sum_{i=1}^n x_i
-- >>> smfmap sumSM test0
-- [1,3,6,10]
sumSM :: SM Int Int Int
sumSM  = simpleSM (\s a -> (s+a, s+a)) 0 

-- | y_n = x_n + 1
-- >>> smfmap plusOneSM test0
-- [2,3,4,5]
plusOneSM :: SM () Int Int
plusOneSM  = simpleSM (\() a -> ((), a + 1)) ()

-- | y_n = x_n * 2
-- >>> smfmap timesTwoSM test0
-- [2,4,6,8]
timesTwoSM :: SM () Int Int
timesTwoSM  = arrSM (\a -> a * 2)

-- combination machine

-- | y_n = \sum_{i=1}^n x_i * 2
-- >>> smfmap ttSM test0
-- [2,6,12,20]
ttSM = timesTwoSM >>>> sumSM 

-- | y_n = (x_n + 1, x_n * 2)
-- >>> smfmap ptSM test0
-- [(2,2),(3,4),(4,6),(5,8)]
ptSM = plusOneSM &&&& timesTwoSM


-- |
-- >>> smfmap mergeOutSM [(2,2),(3,4),(4,6),(5,8)]
-- [4,7,10,13]
--
-- | y_n = (x_n + 1) + (x_n * 2)
-- >>> smfmap ((plusOneSM &&&& timesTwoSM) >>>> mergeOutSM) test0
-- [4,7,10,13]
mergeOutSM :: SM () (Int, Int) Int
mergeOutSM = arrSM (\(a,b)->a+b)


-- | A stack with three operations, push, pop and return the maximum integer in the stack.  
data StackOP 
  = Push Int
  | Pop
  | Max
  deriving Show
  
{-

-- | A naive implementation, and the max operation takes O(n) running time.
pushStk :: [Int] -> Int -> ([Int], Int)
pushStk [] a = ([a], a)
pushStk s  a = (a:s, a)

popStk :: [Int] -> ([Int], Int)
popStk [] = ([], 0)
popStk (x:xs)  = (xs, x)

maxStk :: [Int] -> ([Int], Int)
maxStk [] =  ([], 0) 
maxStk s  = (s, maxList s)  
             
maxList :: [Int] -> Int
maxList [] = 0
maxList (x:xs) = max x (maxList xs) 

initial = []

-}           

-- | A better implementation, and the max operation takes O(1) running time. And we use one more stack to maintain the maximum value.
pushStk :: ([Int], [Int]) -> Int -> (([Int], [Int]), Int)
pushStk ([],[]) a = (([a],[a]), a)
pushStk ((x:xs), (y:ys))  a = if a >= y then ((a:x:xs, a:y:ys), a) else ((a:x:xs, y:ys), a)

popStk :: ([Int], [Int]) -> (([Int], [Int]), Int)
popStk ([],[]) = (([],[]), 0)
popStk ((x:xs), (y:ys)) = if x == y then ((xs, ys), x) else ((xs, y:ys), x)

maxStk :: ([Int], [Int]) -> (([Int], [Int]), Int)
maxStk ([], [])  =  (([], []), 0) 
maxStk ((x:xs), (y:ys)) = (((x:xs), (y:ys)), y)  

initial = ([],[])

-- | switching the alogrithm without touching the code outside.
sf s (Push a) =  pushStk s a
sf s Pop = popStk s
sf s Max =  maxStk s

-- | The stroage type is changing during switching the implementation.
--   There are three ways to handle this issue,
--     1. Remove the type signature of maxSM
--     2. Use hideStorage to hide the storage type
--     3. Use the PartialTypeSignatures extension, leave a hole in the type signature.
--   Personaly, I prefer to use the third one, 
--     because the input type and output type are decidable.
-- >>> smfmap maxSM [Push 5, Push 3, Push 2, Max, Push 7, Max, Pop, Max]
-- [5,3,2,5,7,7,7,5]
maxSM :: SM _ StackOP Int
maxSM = simpleSM sf initial 

-- maxSM :: SM () StackOP Int
-- maxSM = hideStorage $ simpleSM sf initial            

