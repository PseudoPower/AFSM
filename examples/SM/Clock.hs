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

import Control.AFSM
import Control.AFSM.Draft.SMMonoid
import Control.AFSM.Draft.SMP

secondSF :: Int -> SMP Int (Int, Bool)
secondSF init = toSMP "secondSF" $ simpleSM (\s x -> let s' = mod (s+x) 60000; y = div s' 1000 in (s', (y, y == 0))) (init * 1000)

minuteSF :: Int -> SMP Bool (Int, Bool)
minuteSF init = toSMP "minuteSF" $ simpleSM (\s x -> if x then let s' = mod (s + 1) 60 in (s', (s', s' == 0)) else (s, (s, False))) init

hourSF :: Int -> SMP Bool (Int, Bool)
hourSF init = toSMP "hourSF" $ simpleSM (\s x -> if x then let s' = mod (s + 1) 12 in (s', (s', s' == 0)) else (s, (s, False))) init


clockSF :: (Int, Int, Int) -> SMP Int [(Int,Int,Int)]
clockSF (h',m',s') = proc w -> do
  (s,sb) <- secondSF s' -< w
  (m,mb) <- minuteSF m' -< sb
  (h,hb) <- hourSF   h' -< mb
  returnA -< [(h,m,s)]