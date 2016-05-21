-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.SFM.Core
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- {-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE RecursiveDo #-}


module Data.AFSM.SFM.Core where

import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.Fix

import Data.AFSM.SFM.CoreType

-- Category

instance Monad m => Category (SFM m) where
  id = idSFM
  (.) = composeSFM

idSFM :: Monad m => SFM m a a
idSFM = SFM f
  where
    f a = return (idSFM, a)
    
composeSFM :: Monad m => SFM m b c -> SFM m a b -> SFM m a c
composeSFM (SFM f1) (SFM f0) = SFM (f2 f0 f1)
  where
    f2 f0 f1 a = do
      (SFM f0', b) <- f0 a
      (SFM f1', c) <- f1 b
      return (SFM (f2 f0' f1'), c)
      
-- Arrow

instance Monad m => Arrow (SFM m) where
  arr = arrSFM
  first = firstSFM
  second = secondSFM
  (***) = productSFM
  (&&&) = fanoutSFM

arrSFM :: Monad m => (a -> b) -> SFM m a b
arrSFM f = SFM (\a -> return (arrSFM f, f a))

firstSFM :: Monad m => SFM m a b -> SFM m (a, c) (b, c)
firstSFM (SFM f) = SFM (f1 f)
  where
    f1 f (a, c) = do
      (SFM f', b) <- f a
      return (SFM (f1 f'), (b, c))

secondSFM :: Monad m => SFM m a b -> SFM m (c, a) (c, b)
secondSFM (SFM f) = SFM (f1 f)
  where
    f1 f (c, a) = do
      (SFM f', b) <- f a
      return (SFM (f1 f'), (c, b))
        

productSFM :: Monad m => SFM m a b -> SFM m c d -> SFM m (a, c) (b, d)
productSFM (SFM f0) (SFM f1) = SFM (f2 f0 f1)
  where
    f2 f0 f1 (a, c) = do
      (SFM f0', b) <- f0 a
      (SFM f1', d) <- f1 c    
      return (SFM (f2 f0' f1'), (b, d))
      
fanoutSFM :: Monad m => SFM m a b -> SFM m a c -> SFM m a (b, c)
fanoutSFM (SFM f0) (SFM f1) = SFM (f2 f0 f1)
  where
    f2 f0 f1 a = do
      (SFM f0', b) <- f0 a
      (SFM f1', c) <- f1 a
      return (SFM (f2 f0' f1'), (b, c))
      

-- ArrowChoice

instance Monad m => ArrowChoice (SFM m) where
  left = leftSFM
  right = rightSFM
  (+++) = sumSFM
  (|||) = faninSFM

leftSFM :: Monad m => SFM m a b -> SFM m (Either a c) (Either b c)
leftSFM (SFM f0) = SFM (f1 f0)
  where
    f1 f0 (Right c) = return (SFM (f1 f0), Right c)
    f1 f0 (Left a) = do
      (SFM f0', b) <- f0 a
      return (SFM (f1 f0'), Left b)
        

rightSFM :: Monad m => SFM m a b -> SFM m (Either c a) (Either c b)
rightSFM (SFM f0) = SFM (f1 f0)
  where
    f1 f0 (Left c) = return (SFM (f1 f0), Left c)
    f1 f0 (Right a) = do
      (SFM f0', b) <- f0 a
      return (SFM (f1 f0'), Right b)
        

sumSFM :: Monad m => SFM m a b -> SFM m c d -> SFM m (Either a c) (Either b d)
sumSFM (SFM f0) (SFM f1) = SFM (f2 f0 f1)
  where
    f2 f0 f1 (Left a)  = do
      (SFM f0', b) <- f0 a
      return (SFM (f2 f0' f1), Left b)
    f2 f0 f1 (Right c) = do
      (SFM f1', d) <- f1 c
      return (SFM (f2 f0 f1'), Right d)

faninSFM :: Monad m => SFM m a c -> SFM m b c -> SFM m (Either a b) c
faninSFM (SFM f0) (SFM f1) = SFM (f2 f0 f1)
  where
    f2 f0 f1 (Left a)  = do
      (SFM f0', c) <- f0 a
      return (SFM (f2 f0' f1), c)
    f2 f0 f1 (Right b) = do
      (SFM f1', c) <- f1 b
      return (SFM (f2 f0 f1'), c)

-- ArrowApply

instance Monad m => ArrowApply (SFM m) where
  app = appSFM

appSFM :: Monad m => SFM m (SFM m a b, a) b
appSFM = SFM f
  where
    f (SFM f0, a) = do
      (_, b) <- f0 a
      return (SFM f, b)


-- ArrowLoop

instance MonadFix m => ArrowLoop (SFM m) where
    loop = loopSFM

loopSFM :: MonadFix m => SFM m (a, c) (b, c) -> SFM m a b
loopSFM (SFM f0) = SFM (f1 f0)
  where
    f1 f0 a = do
      rec { (SFM f0', (b, c)) <- f0 (a, c) }
      return (SFM (f1 f0'), b)
        

