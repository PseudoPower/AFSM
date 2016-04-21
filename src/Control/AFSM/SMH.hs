{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.SMH
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.SMH (
  
  newSMH,
  simpleSMH,
  
  hideStorage
  
) where

import Control.Category
import Control.Arrow

import Control.AFSM.CoreType
import Control.AFSM.Core

-- | the same constructor with newSM
newSMH :: (() -> a -> (SMH a b, b)) -> SMH a b
newSMH f = newSM f ()

-- | the same constructor with simpleSM
simpleSMH :: (s -> a -> (s, b)) -> s -> SMH a b
simpleSMH f s = newSMH (f' s)
  where
    f' s' () a' = (newSMH (f' s''), b)
      where
        (s'', b) = f s' a' 

-- | hide the Storage type in the transition function.
hideStorage :: SM s a b -> SMH a b
hideStorage (SM (TF f) s) = newSMH (f1 f s)
  where 
    f1 f s () a = (newSMH (f1 f' s'), b)
      where
        (SM (TF f') s', b) = f s a

-- | absorb the right SM and hide its storage.
absorbRSM :: SM s0 a b -> SM s1 b c -> SM s0 a c
absorbRSM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1 s1) s0
  where
    f2 f0 f1 s1 s0 a = (newSM (f2 f0' f1' s1') s0', c)
      where
        (SM (TF f0') s0', b) = f0 s0 a
        (SM (TF f1') s1', c) = f1 s1 b

-- | absorb the left SM and hide its storage.
absorbLSM :: SM s0 a b -> SM s1 b c -> SM s1 a c
absorbLSM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1 s0) s1
  where
    f2 f0 f1 s0 s1 a = (newSM (f2 f0' f1' s0') s1', c)
      where
        (SM (TF f0') s0', b) = f0 s0 a
        (SM (TF f1') s1', c) = f1 s1 b
    
-- Category instance

instance Category (SM ()) where
  id  = idSMH
  (.) = composeSMH

idSMH :: SMH a a
idSMH = newSMH (\_ a -> (idSMH, a))

composeSMH :: SMH b c -> SMH a b -> SMH a c
composeSMH (SM (TF f1) _) (SM (TF f0) _) = newSMH (f2 f0 f1)
  where
    f2 f0 f1 _ a = (newSMH (f2 f0' f1'), c)
      where
        (SM (TF f0') _, b) = f0 () a
        (SM (TF f1') _, c) = f1 () b

        
-- Arrow instance

instance Arrow (SM ()) where
  arr = arrSMH
  first = firstSMH
  second = secondSMH
  (***) = productSMH
  (&&&) = fanoutSMH

arrSMH :: (a -> b) -> SMH a b
arrSMH f = newSMH (\_ a ->(arrSMH f, f a))

firstSMH :: SMH a b -> SMH (a, c) (b, c)
firstSMH (SM (TF f) _) = newSMH (f1 f)
  where
    f1 f _ (a, c) = (newSMH (f1 f'), (b, c))
      where
        (SM (TF f') _, b) = f () a


secondSMH :: SMH a b -> SMH (c, a) (c, b)
secondSMH (SM (TF f) _) = newSMH (f1 f)
  where
    f1 f _ (c, a) = (newSMH (f1 f'), (c, b))
      where
        (SM (TF f') _, b) = f () a

productSMH :: SMH a b -> SMH c d -> SMH (a, c) (b, d)
productSMH (SM (TF f0) _) (SM (TF f1) _) = newSMH (f2 f0 f1)
  where
    f2 f0 f1 _ (a, c) = (newSMH (f2 f0' f1'), (b, d))
      where
        (SM (TF f0') _, b) = f0 () a
        (SM (TF f1') _, d) = f1 () c

fanoutSMH :: SMH a b -> SMH a c -> SMH a (b, c)
fanoutSMH (SM (TF f0) _) (SM (TF f1) _) = newSMH (f2 f0 f1)
  where
    f2 f0 f1 _ a = (newSMH (f2 f0' f1'), (b, c))
      where
        (SM (TF f0') _, b) = f0 () a
        (SM (TF f1') _, c) = f1 () a



-- ArrowChoice

instance ArrowChoice (SM ()) where
  left = leftSMH
  right = rightSMH
  (+++) = sumSMH
  (|||) = faninSMH

leftSMH :: SMH a b -> SMH (Either a c) (Either b c)
leftSMH (SM (TF f0) _) = newSMH (f1 f0)
  where
    f1 f0 _ (Right c) = (newSMH (f1 f0), Right c)
    f1 f0 _ (Left a) = (newSMH (f1 f0'), Left b)
      where
        (SM (TF f0') _, b) = f0 () a

rightSMH :: SMH a b -> SMH (Either c a) (Either c b)
rightSMH (SM (TF f0) _) = newSMH (f1 f0)
  where
    f1 f0 _ (Left c) = (newSMH (f1 f0), Left c)
    f1 f0 _ (Right a) = (newSMH (f1 f0'), Right b)
      where
        (SM (TF f0') _, b) = f0 () a

sumSMH :: SMH a b -> SMH c d -> SMH (Either a c) (Either b d)
sumSMH (SM (TF f0) _) (SM (TF f1) _) = newSMH (f2 f0 f1)
  where
    f2 f0 f1 _ (Left a)  = let (SM (TF f0') _, b) = f0 () a in (newSMH (f2 f0' f1), Left b)
    f2 f0 f1 _ (Right c) = let (SM (TF f1') _, d) = f1 () c in (newSMH (f2 f0 f1'), Right d)

faninSMH :: SMH a c -> SMH b c -> SMH (Either a b) c
faninSMH (SM (TF f0) _) (SM (TF f1) _) = newSMH (f2 f0 f1)
  where
    f2 f0 f1 _ (Left a)  = let (SM (TF f0') _, c) = f0 () a in (newSMH (f2 f0' f1), c)
    f2 f0 f1 _ (Right b) = let (SM (TF f1') _, c) = f1 () b in (newSMH (f2 f0 f1'), c)


-- ArrowApply

instance ArrowApply (SM ()) where
  app = appSM

appSM :: SMH (SMH a b, a) b
appSM = newSMH f
  where
    f _ (SM (TF f0) _, a) = (newSMH f, snd $ f0 () a)


-- ArrowLoop

instance ArrowLoop (SM ()) where
    loop = loopSMH


-- SM has build-in loop structure, but the ArrowLoop instance helps us sharing storage between SMs, and adding one more instance is harmless, :)
loopSMH :: SMH (a, c) (b, c) -> SMH a b
loopSMH (SM (TF f0) _) = newSMH (f1 f0)
  where
    f1 f0 _ a = (newSMH (f1 f0'), b)
      where
        (SM (TF f0') _, (b, c)) = f0 () (a, c)
