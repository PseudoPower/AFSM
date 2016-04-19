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

-- {-# LANGUAGE InstanceSigs #-}

module Control.AFSM.SMFunctor where

-- import Prelude hiding ((.))
-- import Data.Functor.
import Data.Functor.Compose
import Control.Monad

import Control.AFSM.CoreType
import Control.AFSM.Util
-- import Control.AFSM.Core

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
     
-- instance SMFunctor (SM s r) where
--   smexec sm ra = (sm, absorbRSM ra sm)
  
instance SMFunctor (Either a) where
  smexec sm (Left a) = (sm, Left a)
  smexec (SM (TF f) s) (Right b) = (sm', Right c)
    where (sm', c) = f s b
    
instance SMFunctor ((,) a) where
  smexec (SM (TF f) s) (a, b) = (sm', (a, c))
    where (sm', c) = f s b
    
   
smexecSM :: SMFunctor f => SM s a b -> SM s (f a) (f b)
smexecSM (SM (TF f0) s0') = newSM (f1 f0) s0'
  where
    f1 f0 s0 fa = (newSM (f1 f0') s0', fb)
      where 
      ((SM (TF f0') s0'), fb) = smexec (newSM f0 s0) fa
    
-- Advanced functions

smexecSMA :: SMFunctor f => SM s a b -> SM (SM s a b) (f a) (f b)
smexecSMA sm = newSM f sm
  where
    f sm fa = (newSM f sm', fb)
      where 
      (sm', fb) = smexec sm fa  

-- newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (SMFunctor f, SMFunctor g) => SMFunctor (Compose f g) where
  smexec sm fga = (st sm'', Compose fgb)
    where
      sm' = smexecSMA sm
      (sm'', fgb) = smexec sm' $ getCompose fga
      
  
      
-- SMMonad

bindSM :: (Monad m, SMFunctor m) => m a -> SM s a (m b) -> (SM s a (m b), m b)
bindSM ma sm = (sm', join mmb)
  where
    (sm', mmb) = smexec sm ma

(>>>=) :: (Monad m, SMFunctor m) => m a -> SM s a (m b) -> m b
(>>>=) ma sm = join $ smfmap sm ma

{-

-- require WrappedMonad.

class SMMonad m where
  (>>>=) :: m a -> SM s a (m b) -> (SM s a (m b), m b)

instance (Monad m, SMFunctor m) => SMMonad m where
  (>>>=) ma sm = (sm', join mmb)
    where
      (sm', mmb) = smfmap sm ma
      
-}
