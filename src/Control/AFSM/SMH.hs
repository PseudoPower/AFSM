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

-- | hide the Storage type in the transition function.
hideStorage :: SM s a b -> SMH a b
hideStorage (SM f s) = SM (f1 f s) ()
  where 
    f1 f s () a = let (SM f' s', b) = f s a in (SM (f1 f' s') (), b)
    

-- Category instance

instance Category (SM ()) where
  id  = idSMH
  (.) = composeSMH

idSMH :: SMH a a
idSMH = SM (\_ a -> (idSMH, a)) ()

composeSMH :: SMH b c -> SMH a b -> SMH a c
composeSMH sm1 sm0 = hideStorage $ SM f2 (sm0,sm1)
  where
    f2 ((SM f0 s0),(SM f1 s1)) a = (SM f2 (sm0', sm1'), c)
      where
        (sm0', b) = f0 s0 a
        (sm1', c) = f1 s1 b

-- Arrow instance

instance Arrow (SM ()) where
  arr = arrSMH
  first = firstSMH
  second = secondSMH
  (***) = productSMH
  (&&&) = fanoutSMH

arrSMH :: (a -> b) -> SMH a b
arrSMH f =
  SM (\_ a ->(arrSMH f, f a)) ()

firstSMH :: SMH a b -> SMH (a, c) (b, c)
firstSMH sm = hideStorage $ SM f1 sm
  where
    f1 (SM f s) (a,c) = ((SM f1 sm'), (b, c))
      where
        (sm', b) = f s a


secondSMH :: SMH a b -> SMH (c, a) (c, b)
secondSMH sm = hideStorage $ SM f1 sm
  where
    f1 (SM f s) (c,a) = ((SM f1 sm'), (c, b))
      where
        (sm', b) = f s a

productSMH :: SMH a b -> SMH c d -> SMH (a, c) (b, d)
productSMH sm0 sm1 = hideStorage $ SM f2 (sm0, sm1)
  where
    f2 ((SM f0 s0),(SM f1 s1)) (a, c) = (SM f2 (sm0', sm1'), (b, d))
      where
        (sm0', b) = f0 s0 a
        (sm1', d) = f1 s1 c

fanoutSMH :: SMH a b -> SMH a c -> SMH a (b, c)
fanoutSMH sm0 sm1 = hideStorage $ SM f2 (sm0, sm1)
  where
    f2 ((SM f0 s0),(SM f1 s1)) a = (SM f2 (sm0', sm1'), (b, c))
      where
        (sm0', b) = f0 s0 a
        (sm1', c) = f1 s1 a



-- ArrowChoice

instance ArrowChoice (SM ()) where
  left = leftSMH
  right = rightSMH
  (+++) = sumSMH
  (|||) = faninSMH

leftSMH :: SMH a b -> SMH (Either a c) (Either b c)
leftSMH sm = hideStorage $ SM f1 sm
  where
    f1 sm' (Right c) = (SM f1 sm', Right c)
    f1 (SM f0 s0) (Left a) = (SM f1 sm'', Left b)
      where
        (sm'', b) = f0 s0 a

rightSMH :: SMH a b -> SMH (Either c a) (Either c b)
rightSMH sm = hideStorage $ SM f1 sm
  where
    f1 sm' (Left c) = (SM f1 sm', Left c)
    f1 (SM f s) (Right a) = ((SM f1 sm''), Right b)
      where
        (sm'', b) = f s a

sumSMH :: SMH a b -> SMH c d -> SMH (Either a c) (Either b d)
sumSMH sm0 sm1 = hideStorage $ SM f2 (sm0, sm1)
  where
    f2 (SM f0 s0, sm1') (Left a)  = let (sm0', b) = f0 s0 a in (SM f2 (sm0', sm1'), Left b)
    f2 (sm0', SM f1 s1) (Right c) = let (sm1', d) = f1 s1 c in (SM f2 (sm0', sm1'), Right d)

faninSMH :: SMH a c -> SMH b c -> SMH (Either a b) c
faninSMH sm0 sm1 = hideStorage $ SM f2 (sm0, sm1)
  where
    f2 (SM f0 s0, sm1') (Left a)  = let (sm0', c) = f0 s0 a in (SM f2 (sm0', sm1'), c)
    f2 (sm0', SM f1 s1) (Right b) = let (sm1', c) = f1 s1 b in (SM f2 (sm0', sm1'), c)

-- ArrowApply

instance ArrowApply (SM ()) where
  app = appSM

appSM :: SMH (SMH a b, a) b
appSM = SM f1 ()
  where
    f1 () ((SM f s), a) = (SM f1 (), snd $ f s a)


-- ArrowLoop

instance ArrowLoop (SM ()) where
    loop = loopSMH


-- SM has build-in loop structure, but the ArrowLoop instance helps us sharing storage between SMs, and adding one more instance is harmless, :)
loopSMH :: SMH (a, c) (b, c) -> SMH a b
loopSMH sm = hideStorage $ SM f1 sm
  where
    f1 (SM f s) a = (SM f1 sm', b)
      where
        (sm', (b, c)) = f s (a, c)


-- Functor

instance Functor (SM () a) where
  fmap = fmapSMH

-- fmapSM f sm = sm >>> arr f
fmapSMH :: (b -> c) -> SMH a b -> SMH a c
fmapSMH f sm = hideStorage $ SM f1 sm
  where
    f1 (SM f0 s0) a = (SM f1 sm', f b)
      where
        (sm', b) = f0 s0 a

