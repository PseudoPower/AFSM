-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.SMFunctor
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.SMFunctor where

-- import Prelude hiding ((.))
-- import Data.Functor.Identity
import Control.AFSM.CoreType
import Control.AFSM.Core

class SMFunctor f where
  smexec :: SM s a b -> f a -> (SM s a b, f b)
  smfmap :: SM s a b -> f a -> f b
  smfmap sm a = snd $ smexec sm a 

instance SMFunctor [] where
  smexec sm [] = (sm, [])
  smexec (SM (TF f) s) (x:xs) = (sm'', b:bs)
    where
      (sm', b) = f s x
      (sm'', bs) = (smexec sm' xs)

  
instance SMFunctor Maybe where
  smexec sm Nothing = (sm, Nothing)
  smexec (SM (TF f) s) (Just a) = (sm', Just b)
    where (sm', b) = f s a
    
-- instance SMFunctor Identity where
--   smexec sm a = (sm', Identity b)
--     where (sm', b) = step sm (runIdentity a)

instance SMFunctor ((->) r) where
  smexec sm@(SM (TF f) s) ra = (sm, rb)
    where
      rb r = snd $ f s (ra r)
     
instance SMFunctor (SM s r) where
  smexec sm ra = (sm, absorbRSM ra sm)
  
instance SMFunctor (Either a) where
  smexec sm (Left a) = (sm, Left a)
  smexec (SM (TF f) s) (Right b) = (sm', Right c)
    where (sm', c) = f s b
    
instance SMFunctor ((,) a) where
  smexec (SM (TF f) s) (a, b) = (sm', (a, c))
    where (sm', c) = f s b