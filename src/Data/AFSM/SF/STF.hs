-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.SF.STF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.AFSM.SF.STF where


import Control.Category
import Control.Arrow

import Data.AFSM.SF.CoreType

instance Category (STF s) where
  id = idSTF
  (.) = composeSTF
  
  
idSTF :: STF s a a
idSTF = STF f
  where
    f s a = ((STF f, s), a)
    
composeSTF :: STF s b c -> STF s a b -> STF s a c
composeSTF (STF f1) (STF f0) = STF (f2 f0 f1)
  where
    f2 f0 f1 s a = ((STF (f2 f0' f1'), s''), c)
      where
        ((STF f0', s'), b) = f0 s a
        ((STF f1', s''), c) = f1 s' b
        
-- Arrow

instance Arrow (STF s) where
  arr = arrSTF
  first = firstSTF
  second = secondSTF
  (***) = productSTF
  (&&&) = fanoutSTF

arrSTF :: (a -> b) -> STF s a b
arrSTF f = STF f1
  where
    f1 s a = ((STF f1, s), f a)

firstSTF :: STF s a b -> STF s (a, c) (b, c)
firstSTF (STF f0) = STF (f1 f0)
  where
    f1 f0 s (a, c) = ((STF (f1 f0'), s'), (b, c))
      where
        ((STF f0', s'), b) = f0 s a


secondSTF :: STF s a b -> STF s (c, a) (c, b)
secondSTF (STF f0) = STF (f1 f0)
  where
    f1 f0 s (c, a) = ((STF (f1 f0'), s'), (c, b))
      where
        ((STF f0', s'), b) = f0 s a
        

productSTF :: STF s a b -> STF s c d -> STF s (a, c) (b, d)
productSTF (STF f0) (STF f1) = STF (f2 f0 f1)
  where
    f2 f0 f1 s (a, c) = ((STF (f2 f0' f1'), s''), (b, d))
      where
        ((STF f0', s'), b) = f0 s a
        ((STF f1', s''), d) = f1 s' c

fanoutSTF :: STF s a b -> STF s a c -> STF s a (b, c)
fanoutSTF (STF f0) (STF f1) = STF (f2 f0 f1)
  where
    f2 f0 f1 s a = ((STF (f2 f0' f1'), s''), (b, c))
      where
        ((STF f0', s'), b) = f0 s a
        ((STF f1', s''), c) = f1 s' a
        
-- ArrowChoice

instance ArrowChoice (STF s) where
  left = leftSTF
  right = rightSTF
  (+++) = sumSTF
  (|||) = faninSTF
  
leftSTF :: STF s a b -> STF s (Either a c) (Either b c)
leftSTF (STF f0) = STF (f1 f0)
  where
    f1 f0 s (Right c) = ((STF (f1 f0), s), Right c)
    f1 f0 s (Left a) = ((STF (f1 f0'), s'), Left b)
      where
        ((STF f0', s'), b) = f0 s a
        
rightSTF :: STF s a b -> STF s (Either c a) (Either c b)
rightSTF (STF f0) = STF (f1 f0)
  where
    f1 f0 s (Left c) = ((STF (f1 f0), s), Left c)
    f1 f0 s (Right a) = ((STF (f1 f0'), s'), Right b)
      where
        ((STF f0', s'), b) = f0 s a

sumSTF :: STF s a b -> STF s c d -> STF s (Either a c) (Either b d)
sumSTF (STF f0) (STF f1) = STF (f2 f0 f1)
  where
    f2 f0 f1 s (Left a)  = let ((STF f0', s'), b) = f0 s a in ((STF (f2 f0' f1), s'), Left b)
    f2 f0 f1 s (Right c) = let ((STF f1', s'), d) = f1 s c in ((STF (f2 f0 f1'), s'), Right d)

faninSTF :: STF s a c -> STF s b c -> STF s (Either a b) c
faninSTF (STF f0) (STF f1) = STF (f2 f0 f1)
  where
    f2 f0 f1 s (Left a)  = let ((STF f0', s'), c) = f0 s a in ((STF (f2 f0' f1), s'), c)
    f2 f0 f1 s (Right b) = let ((STF f1', s'), c) = f1 s b in ((STF (f2 f0 f1'), s'), c)
    
-- ArrowApply

instance ArrowApply (STF s) where
  app = appSTF

appSTF :: STF s (STF s a b, a) b
appSTF = STF f
  where
    f s (STF f0, a) = ((STF f, s'), b)
      where
        ((STF f0', s'), b) = f0 s a

-- ArrowLoop

instance ArrowLoop (STF s) where
    loop = loopSTF
    
loopSTF :: STF s (a, c) (b, c) -> STF s a b
loopSTF (STF f0) = STF (f1 f0)
  where
    f1 f0 s a = ((STF (f1 f0'), s), b)
      where
        ((STF f0', s'), (b, c)) = f0 s (a, c)


     
