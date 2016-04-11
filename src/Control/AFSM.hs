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
  SM :: s -> (SMState s a b) -> SM a b
  
-- Constructors

-- | newSM is the same with SM constructor.
newSM :: s -> (SMState s a b) -> SM a b
newSM = SM

-- | simpleSM is to build a simple SM which have only one SMState.
simpleSM :: s -> (s -> a -> (s, b)) -> SM a b
simpleSM s f = SM s f'
  where
    f' = (\s' a' -> let (s'', b) = f s' a' in (SM s'' f', b))
    
-- Basic State Machines

-- | constSM build a SM which always return b
constSM :: b -> SM a b
constSM b = SM () f
  where
    f _ a = ((constSM b), b)

-- Category instance    

instance Category SM where
  id  = idSM
  (.) = composeSM
  
idSM :: SM a a
idSM = SM () (\_ a -> (idSM, a))
  
composeSM :: SM b c -> SM a b -> SM a c
composeSM sm1 sm0 = SM (sm0,sm1) f2
  where
    f2 ((SM s0 f0),(SM s1 f1)) a = (SM (sm0', sm1') f2, c)
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
  SM () (\_ a ->(arrSM f, f a))
          
firstSM :: SM a b -> SM (a, c) (b, c)
firstSM sm = SM sm f1
  where
    f1 (SM s f) (a,c) = ((SM sm' f1), (b, c))
      where
        (sm', b) = f s a
        
secondSM :: SM a b -> SM (c, a) (c, b)
secondSM sm = SM sm f1
  where
    f1 (SM s f) (c,a) = ((SM sm' f1), (c, b))
      where
        (sm', b) = f s a

productSM :: SM a b -> SM c d -> SM (a, c) (b, d)
productSM sm0 sm1 = SM (sm0, sm1) f2
  where
    f2 ((SM s0 f0),(SM s1 f1)) (a, c) = (SM (sm0', sm1') f2, (b, d))
      where
        (sm0', b) = f0 s0 a
        (sm1', d) = f1 s1 c

fanoutSM :: SM a b -> SM a c -> SM a (b, c)
fanoutSM sm0 sm1 = SM (sm0, sm1) f2
  where
    f2 ((SM s0 f0),(SM s1 f1)) a = (SM (sm0', sm1') f2, (b, c))
      where
        (sm0', b) = f0 s0 a
        (sm1', c) = f1 s1 a

-- ArrowChoice 

leftSM :: SM a b -> SM (Either a c) (Either b c)
leftSM sm = SM sm f1
  where
    f1 sm' (Right c) = (SM sm' f1, Right c)
    f1 (SM s0 f0) (Left a) = (SM sm'' f1, Left b)
      where
        (sm'', b) = f0 s0 a

rightSM :: SM a b -> SM (Either c a) (Either c b)
rightSM sm = SM sm f1
  where
    f1 sm' (Left c) = (SM sm' f1, Left c)
    f1 (SM s f) (Right a) = ((SM sm'' f1), Right b)
      where
        (sm'', b) = f s a
        
sumSM :: SM a b -> SM c d -> SM (Either a c) (Either b d)
sumSM sm0 sm1 = SM (sm0, sm1) f2
  where
    f2 (SM s0 f0, sm1') (Left a)  = let (sm0', b) = f0 s0 a in (SM (sm0', sm1') f2, Left b)
    f2 (sm0', SM s1 f1) (Right c) = let (sm1', d) = f1 s1 c in (SM (sm0', sm1') f2, Right d)

faninSM :: SM a c -> SM b c -> SM (Either a b) c
faninSM sm0 sm1 = SM (sm0, sm1) f2
  where
    f2 (SM s0 f0, sm1') (Left a)  = let (sm0', c) = f0 s0 a in (SM (sm0', sm1') f2, c)
    f2 (sm0', SM s1 f1) (Right b) = let (sm1', c) = f1 s1 b in (SM (sm0', sm1') f2, c)

instance ArrowChoice SM where
  left = leftSM
  right = rightSM
  (+++) = sumSM
  (|||) = faninSM

-- ArrowLoop
-- SM has build-in loop structure, but adding one more instance is harmless, :)

loopSM :: SM (a, c) (b, c) -> SM a b
loopSM sm = SM sm f1
  where
    f1 (SM s f) a = (SM sm' f1, b)
      where
        (sm', (b, c)) = f s (a, c)

instance ArrowLoop SM where
    loop = loopSM
        
-- Evaluation

-- | execute SM a b with input [a].
exec :: SM a b -> [a] -> (SM a b, [b])
exec sm [] = (sm, [])
exec (SM s f) (x:xs) = (sm'', b:bs)
  where 
    (sm', b) = f s x
    (sm'', bs) = (exec sm' xs)
    
-- High order functions
 
-- | execSM converts SM a b -> SM [a] [b], it is very useful to compose SM a [b] and SM b c to SM a [c].
execSM :: SM a b -> SM [a] [b]
execSM sm = simpleSM sm exec
