-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.FunctorSF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.SF.FunctorSF where

import Control.Category
import Control.Monad

import Data.SF.CoreType
import Data.SF.Core

infixl 1  >>>=
infixr 1  =<<<

class FunctorSF f where
  sfexec :: SF a b -> f a -> (SF a b, f b)
  sfmap :: SF a b -> f a -> f b
  sfmap sf a = snd $ sfexec sf a 

instance FunctorSF [] where
  sfexec sf [] = (sf, [])
  sfexec (SF f) (x:xs) = (sf'', b:bs)
    where
      (sf', b) = f x
      (sf'', bs) = (sfexec sf' xs)

instance FunctorSF Maybe where
  sfexec sf Nothing = (sf, Nothing)
  sfexec (SF f) (Just a) = (sf', Just b)
    where (sf', b) = f a

instance FunctorSF ((->) r) where
  sfexec sf@(SF f) ra = (sf, rb)
    where
      rb r = snd $ f (ra r)
     
instance FunctorSF (SF a) where
  sfexec sf fa = (sf, fa >>> sf)
  
instance FunctorSF (Either a) where
  sfexec sf (Left a) = (sf, Left a)
  sfexec (SF f) (Right b) = (sf', Right c)
    where (sf', c) = f b
    
instance FunctorSF ((,) a) where
  sfexec (SF f) (a, b) = (sf', (a, c))
    where (sf', c) = f b
    
   
execSF :: FunctorSF f => SF a b -> SF (f a) (f b)
execSF sf = SF (f1 sf)
  where
    f1 sf fa = (SF (f1 sf'), fb)
      where 
        (sf', fb) = sfexec sf fa
      


      
sfbind :: (Monad m, FunctorSF m) => m a -> SF a (m b) -> (SF a (m b), m b)
sfbind ma sf = (sf', join mmb)
  where
    (sf', mmb) = sfexec sf ma

(>>>=) :: (Monad m, FunctorSF m) => m a -> SF a (m b) -> m b
(>>>=) ma sf = join $ sfmap sf ma

(=<<<) :: (Monad m, FunctorSF m) => SF a (m b) -> m a -> m b
(=<<<) sf ma = ma >>>= sf

bindSF :: (Monad m, FunctorSF m) => SF a (m b) -> SF (m a) (m b)
bindSF sf = newSF f1 sf
  where
    f1 sf ma = (newSF f1 sf', join mmb)
      where 
        (sf', mmb) = sfexec sf ma
    
