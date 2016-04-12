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
import Control.AFSM

class SMFunctor f where
  smexec :: SM a b -> f a -> (SM a b, f b)
  smfmap :: SM a b -> f a -> f b
  smfmap sm a = snd $ smexec sm a 

instance SMFunctor [] where
  smexec = exec
  
instance SMFunctor Maybe where
  smexec sm Nothing = (sm, Nothing)
  smexec sm (Just a) = (sm', Just b)
    where (sm', b) = step sm a
    
-- instance SMFunctor Identity where
--   smexec sm a = (sm', Identity b)
--     where (sm', b) = step sm (runIdentity a)

instance SMFunctor ((->) r) where
  smexec sm ra = (sm, rb)
    where
      rb r = snd $ step sm (ra r)
     
instance SMFunctor (SM r) where
  smexec sm ra = (sm, composeSM sm ra)
  
instance SMFunctor (Either a) where
  smexec sm (Left a) = (sm, Left a)
  smexec sm (Right b) = (sm', Right c)
    where (sm', c) = step sm b
    
instance SMFunctor ((,) a) where
  smexec sm (a, b) = (sm', (a, c))
    where (sm', c) = step sm b