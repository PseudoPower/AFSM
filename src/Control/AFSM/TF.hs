-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.TF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.TF where

import Control.Category
import Control.Arrow

import Control.AFSM.CoreType
import Control.AFSM.Core

-- newtype TF s a b = TF (s -> a -> (SM s a b, b)

-- | transform `SM t (s, a) (s, b)` to `TF s a b`
transSM2TF :: SM t (s, a) (s, b) -> TF s a b
transSM2TF (SM (TF f) t) = TF (f1 f t)
  where
    f1 f t s a = (newSM (f1 f' t') s', b)
      where
        (SM (TF f') t', (s', b)) = f t (s, a)

-- Category instance

instance Category (TF s) where
  id = idTF
  (.) = composeTF
  
  
idTF :: TF s a a
idTF = TF f
  where
    f s a = (newSM f s, a)
    
composeTF :: TF s b c -> TF s a b -> TF s a c
composeTF (TF f1) (TF f0) = TF (f2 f0 f1)
  where
    f2 f0 f1 s a = (newSM (f2 f0' f1') s'', c)
      where
        (SM (TF f0') s', b) = f0 s a
        (SM (TF f1') s'', c) = f1 s' b   

-- Arrow instance

instance Arrow (TF s) where
  arr = arrTF
  first = firstTF
  second = secondTF
  (***) = productTF
  (&&&) = fanoutTF

  
arrTF :: (a -> b) -> TF s a b
arrTF f = TF f1
  where
    f1 s a = (newSM f1 s, f a)

firstTF :: TF s a b -> TF s (a, c) (b, c)
firstTF (TF f0) = TF (f1 f0)
  where
    f1 f0 s (a, c) = (newSM (f1 f0') s', (b, c))
      where
        (SM (TF f0') s', b) = f0 s a

secondTF :: TF s a b -> TF s (c, a) (c, b)
secondTF (TF f0) = TF (f1 f0)
  where
    f1 f0 s (c, a) = (newSM (f1 f0') s', (c, b))
      where
        (SM (TF f0') s', b) = f0 s a
        

productTF :: TF s a b -> TF s c d -> TF s (a, c) (b, d)
productTF (TF f0) (TF f1) = TF (f2 f0 f1)
  where
    f2 f0 f1 s (a, c) = (newSM (f2 f0' f1') s'', (b, d))
      where
        (SM (TF f0') s', b) = f0 s a
        (SM (TF f1') s'', d) = f1 s' c

fanoutTF :: TF s a b -> TF s a c -> TF s a (b, c)
fanoutTF (TF f0) (TF f1) = TF (f2 f0 f1)
  where
    f2 f0 f1 s a = (newSM (f2 f0' f1') s'', (b, c))
      where
        (SM (TF f0') s', b) = f0 s a
        (SM (TF f1') s'', c) = f1 s' a
        
-- ArrowChoice

instance ArrowChoice (TF s) where
  left = leftTF
  right = rightTF
  (+++) = sumTF
  (|||) = faninTF
  
leftTF :: TF s a b -> TF s (Either a c) (Either b c)
leftTF (TF f0) = TF (f1 f0)
  where
    f1 f0 s (Right c) = (newSM (f1 f0) s, Right c)
    f1 f0 s (Left a) = (newSM (f1 f0') s', Left b)
      where
        (SM (TF f0') s', b) = f0 s a
        
rightTF :: TF s a b -> TF s (Either c a) (Either c b)
rightTF (TF f0) = TF (f1 f0)
  where
    f1 f0 s (Left c) = (newSM (f1 f0) s, Left c)
    f1 f0 s (Right a) = (newSM (f1 f0') s', Right b)
      where
        (SM (TF f0') s', b) = f0 s a

sumTF :: TF s a b -> TF s c d -> TF s (Either a c) (Either b d)
sumTF (TF f0) (TF f1) = TF (f2 f0 f1)
  where
    f2 f0 f1 s (Left a)  = let (SM (TF f0') s', b) = f0 s a in (newSM (f2 f0' f1) s', Left b)
    f2 f0 f1 s (Right c) = let (SM (TF f1') s', d) = f1 s c in (newSM (f2 f0 f1') s', Right d)

faninTF :: TF s a c -> TF s b c -> TF s (Either a b) c
faninTF (TF f0) (TF f1) = TF (f2 f0 f1)
  where
    f2 f0 f1 s (Left a)  = let (SM (TF f0') s', c) = f0 s a in (newSM (f2 f0' f1) s', c)
    f2 f0 f1 s (Right b) = let (SM (TF f1') s', c) = f1 s b in (newSM (f2 f0 f1') s', c)
    
-- ArrowApply

instance ArrowApply (TF s) where
  app = appTF

appTF :: TF s (TF s a b, a) b
appTF = TF f
  where
    f s (TF f0, a) = (newSM f s', b)
      where
        (SM (TF f0') s', b) = f0 s a


-- ArrowLoop

instance ArrowLoop (TF s) where
    loop = loopTF
    
loopTF :: TF s (a, c) (b, c) -> TF s a b
loopTF (TF f0) = TF (f1 f0)
  where
    f1 f0 s a = (newSM (f1 f0') s, b)
      where
        (SM (TF f0') s', (b, c)) = f0 s (a, c)


