-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.CoreType
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.SF.CoreType (
  SF(..),
  STF(..)
)where

data SF a b = SF (a -> (SF a b, b))

-- | the constructor with storage
newSF :: (s -> a -> (SF a b, b)) -> s -> SF a b
newSF f s = SF (f s)

-- | the simple constructor
simpleSF :: (s -> a -> (s, b)) -> s -> SF a b
simpleSF f s = SF (f1 f s)
  where
    f1 f s a = (SF (f1 f s'), b)
      where
        (s', b) = f s a
        
data STF s a b = STF (s -> a -> ((STF s a b, s), b))

transSTF2SF :: STF s a b -> s -> SF a b
transSTF2SF (STF f) s = SF (f1 f s)
  where
    f1 f s a = (SF (f1 f' s'), b)
      where
        ((STF f', s'), b) = f s a