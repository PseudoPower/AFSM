-----------------------------------------------------------------------------
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import Control.Monad

import Data.List

import Data.SF.SFM
import Data.SF.IOSFM


data Op = Stop | Up | Down | Time deriving Eq

data EleEvt = EvStop | EvUp | EvDown | PushBottom Int deriving Eq

type Elevator = SFM IO Op (Maybe EleEvt)

newElevator :: Int -> IO Elevator
newElevator n = simpleSFM f (n,1,Stop)
  where
    f :: (Int, Int, Op) -> Op -> IO ((Int, Int, Op), Maybe EleEvt)
    f (n,x,s) op = do
      if op == Stop then return ((n,x,Stop), Just EvStop)
      else if op == Up then return ((n,x,Up), Nothing)
      else if op == Down then return ((n,x,Down), Nothing)
      else if s == Up then 
        if x+1 <= n then 
          return ((n,x+1,Up),Just EvUp) 
        else 
          return ((n,n,Stop),Just EvStop)
      else if s == Down then 
        if x-1 >= 1 then 
          return ((n,x-1,Down),Just EvDown) 
        else 
          return ((n,1,Stop),Just EvStop)
      else return ((n,x,s), Nothing)


type Scheduler = SFM IO EleEvt (Maybe Op)


pushBottom :: Int -> Int -> [Int] -> [Int]
pushBottom n x xs = 
  if x > 0 then 
    (take (x - 1) xs) ++ [1] ++ (drop x xs)
  else
    (take (n - x - 1) xs) ++ [1] ++ (drop (n - x) xs)

stop :: Op -> Int -> Int -> [Int] -> [Int]
stop op n x xs =
  if op == Up then
    (take (x - 1) xs) ++ [0] ++ (drop x xs)
  else
    (take (n + x - 1) xs) ++ [0] ++ (drop (n + x) xs)
    
check :: Op -> Int -> Int -> [Int] -> Op
check Up n x xs = if xs!!(x-1) == 0 then Up else Stop
check Down n x xs = if xs!!(n+x-1) == 0 then Down else Stop
check Stop n x xs = scheduler x xs

scheduler :: Int -> [Int] -> Op
scheduler x xs = case findIndex (==1) xs of
  Just y -> if y < x then Down else if y > x then Up else Stop
  Nothing -> Stop

newScheduler :: Int -> IO Scheduler
newScheduler n = simpleSFM f (n,1,Stop,replicate (2*n) 0)
  where
    f :: (Int, Int, Op, [Int]) -> EleEvt -> IO ((Int, Int, Op, [Int]), Maybe Op)
    f (n,x,op,xs) e = do
      if e == EvStop then do
        putStrLn $ "stop at " ++ show x
        let op' = check op n x xs in
          return ((n,x,op',stop op n x xs), Just op')
      else if e == EvUp then do
        putStrLn $ "going up " ++ show x
        let op' = check op n x xs in
          return ((n,x,op',xs), Just op')
      else if e == EvDown then do
        putStrLn $ "going down " ++ show x
        let op' = check op n x xs in
          return ((n,x,op',xs), Just op')
      else
        case e of PushBottom t -> do {
          putStrLn $ "push bottom: " ++ (if t > 0 then "Up " ++ show x else "Down " ++ show (-x)) ;
          let xs' = pushBottom n t xs; op' = check op n x xs' in 
            return ((n,x,op',xs'), Just op')
        }

-- timer :: Double -> a -> IO [a]
-- timer = simpleSrcM 

main = do
  return ()
