-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.STF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.SF.STF where


import Control.Category
import Control.Arrow

import Data.SF.CoreType

instance Category (STF s) where
  id = idSTF
  (.) = composeSTF
  
  
idSTF :: STF s a a
idSTF = STF f
  where
    f s a = ((STF f, s), a)
    
composeSTF :: STF s b c -> STF s a b -> STF s a c
composeSTF (STF f1) (STF f0) = STF $ f2 f0 f1
  where
    f2 f0 f1 s a = ((STF (f2 f0' f1'), s''), c)
      where
        ((STF f0', s'), b) = f0 s a
        ((STF f1', s''), c) = f1 s' b
        

-- Arrow

instance Arrow (STF s) where
  arr = arrSTF
  first = firstSTF
  
arrSTF :: (a -> b) -> STF s a b
arrSTF f = STF f1
  where
    f1 s a = ((STF f1, s), f a)

firstSTF :: STF s a b -> STF s (a, c) (b, c)
firstSTF (STF f) = STF $ f1 f
  where
    f1 f s (a, c) = ((STF (f1 f'), s'), (b, c))
      where
        ((STF f', s'), b) = f s a

        