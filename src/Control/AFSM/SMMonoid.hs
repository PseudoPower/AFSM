-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.SMMonoid
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.SMMonoid where

import Data.Monoid
import Control.Category
import Control.Arrow

import Control.AFSM.CoreType
import Control.AFSM.Core


class SMMonoid a where
  smempty :: Int -> a
  smid :: Int -> a -> a
  smappend :: Int -> a -> a -> a

{-
instance Monoid a => SMMonoid a where
  smempty _ = mempty
  smid _ a = a
  smappend _ = mappend  
-}

instance SMMonoid () where
  smempty _ = ()
  smid _ _ = ()
  smappend _ _ _= ()
  
-- smepty
-- 1 idSMM
-- 2 arrSMM

-- smid
-- 1 firstSMM
-- 2 secondSMM

-- smappend
-- 1 composeSMM
-- 2 productSMM
-- 3 fanoutSMM

        
-- Category instance

instance SMMonoid s => Category (SM s) where
  id  = idSMM
  (.) = composeSMM

idSMM :: SMMonoid s => SM s a a
idSMM = newSM (\s a -> (idSMM, a)) (smempty 1)
  
composeSMM :: SMMonoid s => SM s b c -> SM s a b -> SM s a c
composeSMM (SM (TF f1) s1) (SM (TF f0) s0) = newSM (f2 f0 f1 s0 s1) $ smappend 1 s0 s1
  where
    f2 f0 f1 s0 s1 _ a = (newSM (f2 f0' f1' s0' s1') $ smappend 1 s0' s1', c)
      where
        (SM (TF f0') s0', b) = f0 s0 a
        (SM (TF f1') s1', c) = f1 s1 b
        

-- Arrow instance

instance SMMonoid s => Arrow (SM s) where
  arr = arrSMM
  first = firstSMM
  second = secondSMM
  (***) = productSMM
  (&&&) = fanoutSMM

arrSMM :: SMMonoid s => (a -> b) -> SM s a b
arrSMM f =
  newSM (\_ a ->(arrSMM f, f a)) (smempty 2)

firstSMM :: SMMonoid s => SM s a b -> SM s (a, c) (b, c)
firstSMM (SM (TF f) s) = newSM (f1 f) (smid 1 s)
  where
    f1 f s (a, c) = (newSM (f1 f') s', (b, c))
      where
        (SM (TF f') s', b) = f s a

secondSMM :: SMMonoid s => SM s a b -> SM s (c, a) (c, b)
secondSMM (SM (TF f) s) = newSM (f1 f) (smid 2 s)
  where
    f1 f s (c, a) = (newSM (f1 f') s', (c, b))
      where
        (SM (TF f') s', b) = f s a

productSMM :: SMMonoid s => SM s a b -> SM s c d -> SM s (a, c) (b, d)
productSMM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1 s0 s1) $ smappend 2 s0 s1
  where
    f2 f0 f1 s0 s1 _ (a, c) = (newSM (f2 f0' f1' s0' s1') $ smappend 2 s0' s1', (b, d))
      where
        ((SM (TF f0') s0'), b) = f0 s0 a
        ((SM (TF f1') s1'), d) = f1 s1 c
        
fanoutSMM :: SMMonoid s => SM s a b -> SM s a c -> SM s a (b, c)
fanoutSMM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1 s0 s1) $ smappend 3 s0 s1
  where
    f2 f0 f1 s0 s1 _ a = (newSM (f2 f0' f1' s0' s1') $ smappend 3 s0' s1', (b, c))
      where
        ((SM (TF f0') s0'), b) = f0 s0 a
        ((SM (TF f1') s1'), c) = f1 s1 a
