-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.SFM
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- {-# LANGUAGE ExistentialQuantification #-}

module Data.SF.SFM where

import Control.Category
import Control.Arrow
import Control.Monad

-- data SFM m a b = forall m. (Monad m) => SFM (a -> m (SFM m a b, b))
newtype SFM m a b = SFM (a -> m (SFM m a b, b))

newSFM :: (Monad m) => (s -> a -> m (SFM m a b, b)) -> s -> m (SFM m a b)
newSFM f s = return (SFM (f s))

simpleSFM :: (Monad m) => (s -> a -> m (s, b)) -> s -> m (SFM m a b)
simpleSFM f0 s = return (SFM (f1 f0 s))
  where
    f1 f0 s a = do
      (s', b) <- (f0 s a)
      return (SFM (f1 f0 s'), b)

-- newtype STFM m s a b = STFM (s -> a -> m ((STFM m s a b, s), b))





simpleSrcM :: (Monad m) => SFM m () a -> m [a]
simpleSrcM (SFM f) = do
  (sfm, a) <- f ()
  xs <- simpleSrcM sfm
  return (a:xs) 





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
      
