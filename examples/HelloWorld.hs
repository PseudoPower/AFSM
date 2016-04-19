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

-- {-# LANGUAGE PartialTypeSignatures #-}

module HelloWorld where

import Control.AFSM
import Control.AFSM.SMH

import Data.Maybe

-- basic machines
test0 = [1, 2, 3, 4]

sumSM :: SM Int Int Int
sumSM  = simpleSM (\s a -> (s+a, s+a)) 0 

ret0 = smfmap sumSM test0

plusOneSM :: SM () Int Int
plusOneSM  = simpleSM (\() a -> ((), a + 1)) ()

ret1 = smfmap plusOneSM test0

timesTwoSM :: SM () Int Int
timesTwoSM  = arrSM (\a -> a * 2)

-- combination machine
ttSM = timesTwoSM >>>> sumSM 

ret2 = smfmap ttSM test0

ptSM = plusOneSM &&&& timesTwoSM

ret3 = smfmap ptSM test0

mergeOutSM :: SM () (Int, Int) Int
mergeOutSM = simpleSM (\s (a,b)->(s, a+b)) ()

-- example 3
data StackOP = Push Int
             |Pop
             |Max
             deriving Show
             

pushStk [] a = ([a], a)
pushStk s  a = (a:s, a)

popStk [] = ([], 0)
popStk (x:xs)  = (xs, x)

maxStk [] =  ([], 0) 
maxStk s  = (s, maxList s)  
             
maxList :: [Int] -> Int
maxList [] = 0
maxList (x:xs) = max x (maxList xs) 

initial = []
             
{-
pushStk ([],[]) a = (([a],[a]), a)
pushStk ((x:xs), (y:ys))  a = if a >= y then ((a:x:xs, a:y:ys), a) else ((a:x:xs, y:ys), a)

popStk ([],[]) = (([],[]), 0)
popStk ((x:xs), (y:ys)) = if x == y then ((xs, ys), x) else ((xs, y:ys), x)

maxStk ([], [])  =  (([], []), 0) 
maxStk ((x:xs), (y:ys)) = (((x:xs), (y:ys)), y)  

initial = ([],[])
-}
sf s (Push a) =  pushStk s a
sf s Pop = popStk s
sf s Max =  maxStk s


maxSM :: SM () StackOP Int
maxSM = hideStorage $ simpleSM sf initial 
 


  
ret4 = smfmap maxSM [Push 5, Push 3, Push 2, Max, Push 7, Max, Pop, Max]
           

