{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Arrowized functional state machines.
--
--   This module is inspired by Yampa and the paper 
--   /Functional Reactive Programming, Continued*/ written by
--     Henrik Nilsson, Antony Courtney and John Peterson.
-----------------------------------------------------------------------------

module Control.AFSM (
  module Control.Arrow,
  
  Event(..),
  
  -- * The 'SM' type
  SM,
  
  -- * The 'SMState' type
  SMState,
  
  -- * Constructors
  newSM,
  simpleSM,
  
  -- * Basic State Machines
  constSM,
  
  -- * High order functions
  execSM,
  
  -- * Evaluation
  exec
  
) where

import Control.Category
import Control.Arrow

import Control.AFSM.Event


type SMState s a b = (s -> a -> (SM a b, b))

-- | 'SM' is a type representing a state machine.
data SM a b where 
  SM :: (SMState s a b) -> s -> SM a b
  
-- Constructors

-- | the SM constructor.
newSM :: (SMState s a b) -> s -> SM a b
newSM = SM

-- | build a simple SM which have only one SMState.
simpleSM :: (s -> a -> (s, b)) -> s -> SM a b
simpleSM f s = SM f' s
  where
    f' = (\s' a' -> let (s'', b) = f s' a' in (SM f' s'', b))
    
-- Basic State Machines

-- | constSM build a SM which always return b
constSM :: b -> SM a b
constSM b = SM f ()
  where
    f _ a = ((constSM b), b)

-- Category instance    

instance Category SM where
  id  = idSM
  (.) = composeSM
  
idSM :: SM a a
idSM = SM (\_ a -> (idSM, a)) ()
  
composeSM :: SM b c -> SM a b -> SM a c
composeSM sm1 sm0 = SM f2 (sm0,sm1)
  where
    f2 ((SM f0 s0),(SM f1 s1)) a = (SM f2 (sm0', sm1'), c)
      where
        (sm0', b) = f0 s0 a
        (sm1', c) = f1 s1 b

        
-- Arrow instance
  
instance Arrow SM where
  arr = arrSM
  first = firstSM
  second = secondSM
  (***) = productSM
  (&&&) = fanoutSM

arrSM :: (a -> b) -> SM a b
arrSM f =
  SM (\_ a ->(arrSM f, f a)) ()
          
firstSM :: SM a b -> SM (a, c) (b, c)
firstSM sm = SM f1 sm
  where
    f1 (SM f s) (a,c) = ((SM f1 sm'), (b, c))
      where
        (sm', b) = f s a
        
secondSM :: SM a b -> SM (c, a) (c, b)
secondSM sm = SM f1 sm
  where
    f1 (SM f s) (c,a) = ((SM f1 sm'), (c, b))
      where
        (sm', b) = f s a

productSM :: SM a b -> SM c d -> SM (a, c) (b, d)
productSM sm0 sm1 = SM f2 (sm0, sm1)
  where
    f2 ((SM f0 s0),(SM f1 s1)) (a, c) = (SM f2 (sm0', sm1'), (b, d))
      where
        (sm0', b) = f0 s0 a
        (sm1', d) = f1 s1 c

fanoutSM :: SM a b -> SM a c -> SM a (b, c)
fanoutSM sm0 sm1 = SM f2 (sm0, sm1)
  where
    f2 ((SM f0 s0),(SM f1 s1)) a = (SM f2 (sm0', sm1'), (b, c))
      where
        (sm0', b) = f0 s0 a
        (sm1', c) = f1 s1 a

-- ArrowChoice 

leftSM :: SM a b -> SM (Either a c) (Either b c)
leftSM sm = SM f1 sm
  where
    f1 sm' (Right c) = (SM f1 sm', Right c)
    f1 (SM f0 s0) (Left a) = (SM f1 sm'', Left b)
      where
        (sm'', b) = f0 s0 a

rightSM :: SM a b -> SM (Either c a) (Either c b)
rightSM sm = SM f1 sm
  where
    f1 sm' (Left c) = (SM f1 sm', Left c)
    f1 (SM f s) (Right a) = ((SM f1 sm''), Right b)
      where
        (sm'', b) = f s a
        
sumSM :: SM a b -> SM c d -> SM (Either a c) (Either b d)
sumSM sm0 sm1 = SM f2 (sm0, sm1)
  where
    f2 (SM f0 s0, sm1') (Left a)  = let (sm0', b) = f0 s0 a in (SM f2 (sm0', sm1'), Left b)
    f2 (sm0', SM f1 s1) (Right c) = let (sm1', d) = f1 s1 c in (SM f2 (sm0', sm1'), Right d)

faninSM :: SM a c -> SM b c -> SM (Either a b) c
faninSM sm0 sm1 = SM f2 (sm0, sm1)
  where
    f2 (SM f0 s0, sm1') (Left a)  = let (sm0', c) = f0 s0 a in (SM f2 (sm0', sm1'), c)
    f2 (sm0', SM f1 s1) (Right b) = let (sm1', c) = f1 s1 b in (SM f2 (sm0', sm1'), c)

instance ArrowChoice SM where
  left = leftSM
  right = rightSM
  (+++) = sumSM
  (|||) = faninSM

-- ArrowLoop
-- SM has build-in loop structure, but adding one more instance is harmless, :)

loopSM :: SM (a, c) (b, c) -> SM a b
loopSM sm = SM f1 sm
  where
    f1 (SM f s) a = (SM f1 sm', b)
      where
        (sm', (b, c)) = f s (a, c)

instance ArrowLoop SM where
    loop = loopSM
    
-- Functor

fmapSM :: (b -> c) -> SM a b -> SM a c
fmapSM f sm = SM f1 sm
  where
    f1 (SM f0 s0) a = (SM f1 sm', f b)
      where
        (sm', b) = f0 s0 a

instance Functor (SM a) where
  fmap = fmapSM
        
-- Evaluation

-- | execute SM a b with input [a].
exec :: SM a b -> [a] -> (SM a b, [b])
exec sm [] = (sm, [])
exec (SM f s) (x:xs) = (sm'', b:bs)
  where 
    (sm', b) = f s x
    (sm'', bs) = (exec sm' xs)
    
-- High order functions
 
-- | converts SM a b -> SM [a] [b], it is very useful to compose SM a [b] and SM b c to SM a [c].
execSM :: SM a b -> SM [a] [b]
execSM sm = simpleSM exec sm
