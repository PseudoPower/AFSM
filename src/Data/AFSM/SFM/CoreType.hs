-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.SFM.CoreType
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.AFSM.SFM.CoreType where

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

