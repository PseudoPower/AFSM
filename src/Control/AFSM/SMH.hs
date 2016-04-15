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

module Control.AFSM.SMH where

import Control.Category
import Control.Arrow

import Control.AFSM.CoreType
import Control.AFSM.Core

newSMH :: (() -> a -> (SMH a b, b)) -> SMH a b
newSMH f = newSM f ()

-- | hide the Storage type in the transition function.
hideStorage :: SM s a b -> SMH a b
hideStorage (SM (TF f) s) = newSMH (f1 f s)
  where 
    f1 f s () a = let (sm', b) = f s a in (newSMH $ f1 (tf sm') (st sm'), b)

    
-- Category instance

instance Category (SM ()) where
  id  = idSMH
  (.) = composeSMH

idSMH :: SMH a a
idSMH = newSMH (\_ a -> (idSMH, a))

composeSMH :: SMH b c -> SMH a b -> SMH a c
composeSMH sm1 sm0 = newSMH $ f2 (tf sm0) (tf sm1)
  where
    f2 f0 f1 _ a = (newSMH $ f2 (tf sm0') (tf sm1'), c)
      where
        (sm0', b) = f0 () a
        (sm1', c) = f1 () b

        
-- Arrow instance

instance Arrow (SM ()) where
  arr = arrSMH
  first = firstSMH
  second = secondSMH
  (***) = productSMH
  (&&&) = fanoutSMH

arrSMH :: (a -> b) -> SMH a b
arrSMH f =
  newSMH (\_ a ->(arrSMH f, f a))

firstSMH :: SMH a b -> SMH (a, c) (b, c)
firstSMH sm = newSMH $ f1 (tf sm)
  where
    f1 f _ (a,c) = (newSMH $ f1 (tf sm'), (b, c))
      where
        (sm', b) = f () a


secondSMH :: SMH a b -> SMH (c, a) (c, b)
secondSMH sm = newSMH $ f1 (tf sm)
  where
    f1 f _ (c,a) = (newSMH $ f1 (tf sm'), (c, b))
      where
        (sm', b) = f () a

productSMH :: SMH a b -> SMH c d -> SMH (a, c) (b, d)
productSMH sm0 sm1 = newSMH $ f2 (tf sm0) (tf sm1)
  where
    f2 f0 f1 _ (a, c) = (newSMH $ f2 (tf sm0') (tf sm1'), (b, d))
      where
        (sm0', b) = f0 () a
        (sm1', d) = f1 () c

fanoutSMH :: SMH a b -> SMH a c -> SMH a (b, c)
fanoutSMH sm0 sm1 = newSMH $ f2 (tf sm0) (tf sm1)
  where
    f2 f0 f1 _ a = (newSMH $ f2 (tf sm0') (tf sm1'), (b, c))
      where
        (sm0', b) = f0 () a
        (sm1', c) = f1 () a



-- ArrowChoice

instance ArrowChoice (SM ()) where
  left = leftSMH
  right = rightSMH
  (+++) = sumSMH
  (|||) = faninSMH

leftSMH :: SMH a b -> SMH (Either a c) (Either b c)
leftSMH sm0 = newSMH $ f1 (tf sm0)
  where
    f1 f0 _ (Right c) = (newSMH $ f1 f0, Right c)
    f1 f0 _ (Left a) = (newSMH $ f1 (tf sm0'), Left b)
      where
        (sm0', b) = f0 () a

rightSMH :: SMH a b -> SMH (Either c a) (Either c b)
rightSMH sm0 = newSMH $ f1 (tf sm0)
  where
    f1 f0 _ (Left c) = (newSMH $ f1 f0, Left c)
    f1 f0 _ (Right a) = (newSMH $ f1 (tf sm0'), Right b)
      where
        (sm0', b) = f0 () a

sumSMH :: SMH a b -> SMH c d -> SMH (Either a c) (Either b d)
sumSMH sm0 sm1 = newSMH (f2 (tf sm0) (tf sm1))
  where
    f2 f0 f1 _ (Left a)  = let (sm0', b) = f0 () a in (newSMH (f2 (tf sm0') f1), Left b)
    f2 f0 f1 _ (Right c) = let (sm1', d) = f1 () c in (newSMH (f2 f0 (tf sm1')), Right d)

faninSMH :: SMH a c -> SMH b c -> SMH (Either a b) c
faninSMH sm0 sm1 = newSMH (f2 (tf sm0) (tf sm1))
  where
    f2 f0 f1 _ (Left a)  = let (sm0', c) = f0 () a in (newSMH (f2 (tf sm0') f1), c)
    f2 f0 f1 _ (Right b) = let (sm1', c) = f1 () b in (newSMH (f2 f0 (tf sm1')), c)


-- ArrowApply

instance ArrowApply (SM ()) where
  app = appSM

appSM :: SMH (SMH a b, a) b
appSM = newSMH f
  where
    f _ (sm, a) = (newSMH f, snd $ (tf sm) () a)


-- ArrowLoop

instance ArrowLoop (SM ()) where
    loop = loopSMH


-- SM has build-in loop structure, but the ArrowLoop instance helps us sharing storage between SMs, and adding one more instance is harmless, :)
loopSMH :: SMH (a, c) (b, c) -> SMH a b
loopSMH sm = newSMH $ f1 (tf sm)
  where
    f1 f0 _ a = (newSMH $ f1 (tf sm'), b)
      where
        (sm', (b, c)) = f0 () (a, c)


-- Functor

instance Functor (SM () a) where
  fmap = fmapSMH

-- fmapSM f sm = sm >>> arr f
fmapSMH :: (b -> c) -> SMH a b -> SMH a c
fmapSMH f sm = newSMH $ f1 (tf sm)
  where
    f1 f0 () a = (newSMH $ f1 (tf sm'), f b)
      where
        (sm', b) = f0 () a
