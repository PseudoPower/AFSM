-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.SF.Core
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Data.AFSM.SF.Core where

import Control.Category
import Control.Arrow

import Data.AFSM.SF.CoreType

-- | Source

buildSrc :: SF a a -> [a]
buildSrc (SF f) = a : (buildSrc sf')
  where
    (sf', a) = f a
    
simpleSrc :: SF () a -> [a]
simpleSrc (SF f) = a : (simpleSrc sf')
  where
    (sf', a) = f ()


-- | Basic SF    

constSF :: b -> SF a b
constSF b = SF (\a -> (constSF b, b))

delaySF :: a -> SF a a
delaySF a = newSF f a
  where
    f s' a' = ((newSF f a'), s')

foldlSF :: (s -> a -> s) -> s -> SF a s
foldlSF f s = newSF f' s
  where
    f' s' a' = (newSF f' s'', s'')
      where
        s'' = f s' a'

foldlDelaySF :: (s -> a -> s) -> s -> SF a s
foldlDelaySF f s = newSF f' s
  where
    f' s' a' = (newSF f' s'', s')
      where
        s'' = f s' a'

        
-- | Event SF

holdSF :: a -> SF (Event a) a
holdSF a = newSF f a
  where
    f a NoEvent = (newSF f a, a)
    f a (Event a') = (newSF f a', a')
    
dropSF :: Eq a => SF a (Event a)
dropSF = newSF f NoEvent
  where
    f NoEvent a = (newSF f (Event a), Event a)
    f (Event a) a' = if a == a' then (newSF f (Event a), NoEvent) else (newSF f (Event a'), Event a')

filterSF :: (a -> Bool) -> SF a (Event a)
filterSF chk = newSF f chk
  where
    f chk a = (newSF f chk, if chk a then Event a else NoEvent)



    
-- Category instance

instance Category SF where
  id  = idSF
  (.) = composeSF

idSF :: SF a a
idSF = SF (\a -> (idSF, a))

composeSF :: SF b c -> SF a b -> SF a c
composeSF (SF f1) (SF f0) = SF (f2 f0 f1)
  where
    f2 f0 f1 a = (SF (f2 f0' f1'), c)
      where
        (SF f0', b) = f0 a
        (SF f1', c) = f1 b

        
-- Arrow instance

instance Arrow SF where
  arr = arrSF
  first = firstSF
  second = secondSF
  (***) = productSF
  (&&&) = fanoutSF

arrSF :: (a -> b) -> SF a b
arrSF !f = SF (\a ->(arrSF f, f a))

firstSF :: SF a b -> SF (a, c) (b, c)
firstSF (SF !f) = SF (f1 f)
  where
    f1 !f (a, c) = (SF (f1 f'), (b, c))
      where
        (SF !f', b) = f a


secondSF :: SF a b -> SF (c, a) (c, b)
secondSF (SF !f) = SF (f1 f)
  where
    f1 !f (c, a) = (SF (f1 f'), (c, b))
      where
        (SF !f', b) = f a

productSF :: SF a b -> SF c d -> SF (a, c) (b, d)
productSF (SF !f0) (SF !f1) = SF (f2 f0 f1)
  where
    f2 !f0 !f1 (a, c) = (SF (f2 f0' f1'), (b, d))
      where
        (SF !f0', b) = f0 a
        (SF !f1', d) = f1 c

fanoutSF :: SF a b -> SF a c -> SF a (b, c)
fanoutSF (SF !f0) (SF !f1) = SF (f2 f0 f1)
  where
    f2 !f0 !f1 a = (SF (f2 f0' f1'), (b, c))
      where
        (SF !f0', b) = f0 a
        (SF !f1', c) = f1 a



-- ArrowChoice

instance ArrowChoice SF where
  left = leftSF
  right = rightSF
  (+++) = sumSF
  (|||) = faninSF

leftSF :: SF a b -> SF (Either a c) (Either b c)
leftSF (SF f0) = SF (f1 f0)
  where
    f1 f0 (Right c) = (SF (f1 f0), Right c)
    f1 f0 (Left a) = (SF (f1 f0'), Left b)
      where
        (SF f0', b) = f0 a

rightSF :: SF a b -> SF (Either c a) (Either c b)
rightSF (SF f0) = SF (f1 f0)
  where
    f1 f0 (Left c) = (SF (f1 f0), Left c)
    f1 f0 (Right a) = (SF (f1 f0'), Right b)
      where
        (SF f0', b) = f0 a

sumSF :: SF a b -> SF c d -> SF (Either a c) (Either b d)
sumSF (SF f0) (SF f1) = SF (f2 f0 f1)
  where
    f2 f0 f1 (Left a)  = let (SF f0', b) = f0 a in (SF (f2 f0' f1), Left b)
    f2 f0 f1 (Right c) = let (SF f1', d) = f1 c in (SF (f2 f0 f1'), Right d)

faninSF :: SF a c -> SF b c -> SF (Either a b) c
faninSF (SF f0) (SF f1) = SF (f2 f0 f1)
  where
    f2 f0 f1 (Left a)  = let (SF f0', c) = f0 a in (SF (f2 f0' f1), c)
    f2 f0 f1 (Right b) = let (SF f1', c) = f1 b in (SF (f2 f0 f1'), c)


-- ArrowApply

instance ArrowApply SF where
  app = appSF

appSF :: SF (SF a b, a) b
appSF = SF f
  where
    f (SF f0, a) = (SF f, snd $ f0 a)


-- ArrowLoop

instance ArrowLoop SF where
    loop = loopSF

loopSF :: SF (a, c) (b, c) -> SF a b
loopSF (SF f0) = SF (f1 f0)
  where
    f1 f0 a = (SF (f1 f0'), b)
      where
        (SF f0', (b, c)) = f0 (a, c)
