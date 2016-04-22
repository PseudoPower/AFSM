-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.SFunctor
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.SF.SFunctor where

import Control.Category
import Control.Monad

import Data.SF.CoreType
import Data.SF.Core

class SFunctor f where
  sfexec :: SF a b -> f a -> (SF a b, f b)
  sfmap :: SF a b -> f a -> f b
  sfmap sf a = snd $ sfexec sf a 

instance SFunctor [] where
  sfexec sf [] = (sf, [])
  sfexec (SF f) (x:xs) = (sf'', b:bs)
    where
      (sf', b) = f x
      (sf'', bs) = (sfexec sf' xs)

instance SFunctor Maybe where
  sfexec sf Nothing = (sf, Nothing)
  sfexec (SF f) (Just a) = (sf', Just b)
    where (sf', b) = f a

instance SFunctor ((->) r) where
  sfexec sf@(SF f) ra = (sf, rb)
    where
      rb r = snd $ f (ra r)
     
instance SFunctor (SF a) where
  sfexec sf fa = (sf, fa >>> sf)
  
instance SFunctor (Either a) where
  sfexec sf (Left a) = (sf, Left a)
  sfexec (SF f) (Right b) = (sf', Right c)
    where (sf', c) = f b
    
instance SFunctor ((,) a) where
  sfexec (SF f) (a, b) = (sf', (a, c))
    where (sf', c) = f b
    
   
execSF :: SFunctor f => SF a b -> SF (f a) (f b)
execSF sf = SF (f1 sf)
  where
    f1 sf fa = (SF (f1 sf'), fb)
      where 
        (sf', fb) = sfexec sf fa
      

bindSF :: (Monad m, SFunctor m) => m a -> SF a (m b) -> (SF a (m b), m b)
bindSF ma sf = (sf', join mmb)
  where
    (sf', mmb) = sfexec sf ma

(>>>=) :: (Monad m, SFunctor m) => m a -> SF a (m b) -> m b
(>>>=) ma sf = join $ sfmap sf ma
