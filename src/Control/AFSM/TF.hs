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

-- newtype TF s a b = TF (s -> a -> (SM s a b, b))

-- Category instance

instance Category (TF s) where
  id = idTF
  (.) = composeTF
  
  
idTF :: TF s a a
idTF = TF f
  where
    f s a = (newSM f s, a)
    
composeTF :: TF s b c -> TF s a b -> TF s a c
composeTF (TF f1) (TF f0) = TF $ f2 f0 f1
  where
    f2 f0 f1 s a = (newSM (f2 (tf sm0) (tf sm1)) (st sm1), c)
      where
        (sm0, b) = f0 s a
        (sm1, c) = f1 (st sm0) b
        

-- Arrow instance

instance Arrow (TF s) where
  arr = arrTF
  first = firstTF
  
arrTF :: (a -> b) -> TF s a b
arrTF f = TF f1
  where
    f1 s a = (newSM f1 s, f a)

firstTF :: TF s a b -> TF s (a, c) (b, c)
firstTF (TF f) = TF $ f1 f
  where
    f1 f s (a, c) = (newSM (f1 (tf sm)) (st sm), (b, c))
      where
        (sm, b) = f s a

        
-- ArrowChoice

-- ArrowApply

-- ArrowLoop
