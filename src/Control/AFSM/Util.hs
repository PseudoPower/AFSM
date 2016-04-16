-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.Util
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.Util where

import Control.AFSM.CoreType 

f1template :: (a1 -> a0) -> (b0 -> b1)
           -> (s -> a0 -> (SM s a0 b0, b0)) 
           -> s -> a1 -> (SM s a1 b1, b1)
f1template from to f0 s a1 =  (newSM (f1template from to f0') s', (to b0))
  where
    a0 = from a1
    (sm0, b0) = f0 s a0
    f0' = tf sm0
    s' = st sm0

f2template :: (a2 -> (a0, a1)) -> (b0 -> b1 -> b2) 
           -> (s0 -> a0 -> (SM s0 a0 b0, b0)) -> (s1 -> a1 -> (SM s1 a1 b1, b1)) 
           -> (s0,s1) -> a2 -> (SM (s0,s1) a2 b2, b2)
f2template from to f0 f1 (s0, s1) a2 = (newSM (f2template from to f0' f1') (s0', s1'), (to b0 b1))
  where
    (a0, a1) = from a2
    (sm0, b0) = f0 s0 a0
    (sm1, b1) = f1 s1 a1
    f0' = tf sm0
    s0' = st sm0
    f1' = tf sm1
    s1' = st sm1


absorb :: (a1 -> a0) -> (b0 -> b1) -> SM s a0 b0 -> SM s a1 b1
absorb from to sm0 = newSM (f1template from to (tf sm0)) (st sm0)

merge :: (a2 -> (a0, a1)) -> (b0 -> b1 -> b2)
      -> SM s0 a0 b0 -> SM s1 a1 b1 -> SM (s0,s1) a2 b2
merge from to sm0 sm1 = newSM (f2template from to (tf sm0) (tf sm1)) (st sm0, st sm1)