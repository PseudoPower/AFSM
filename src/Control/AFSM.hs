{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  
-- Copyright   :  (c) Hanzhong Xu 2016,
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
  
  -- * The 'SM' type
  SM,
  
  -- * The 'SMState' type
  SMState,
  
  -- * Constructors
  newSM,
  simpleSM,
  
  -- * High order functions
  execSM,
  
  -- * Evaluation
  exec
  
) where

import Control.Category
import Control.Arrow

type SMState r a b = (r -> a -> (SM a b, b))

-- | 'SM' is a type representing a state machine.
data SM a b where 
  SM :: r -> (SMState r a b) -> SM a b
  
-- Constructors

newSM :: r -> (SMState r a b) -> SM a b
newSM = SM

simpleSM :: r -> (r -> a -> (r, b)) -> SM a b
simpleSM r f = SM r f'
  where
    f' = (\r' a' -> let (r'', b) = f r' a' in (SM r'' f', b))

-- Category instance    

instance Category SM where
  id  = idSM
  (.) = composeSM
  
idSM :: SM a a
idSM = SM () (\_ a -> (idSM, a))
  
composeSM :: SM b c -> SM a b -> SM a c
composeSM sm1 sm0 = SM (sm0,sm1) f2
  where
    f2 ((SM r0 f0),(SM r1 f1)) a = (SM (sm0', sm1') f2, c)
      where
        (sm0', b) = f0 r0 a
        (sm1', c) = f1 r1 b

        
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
    f1 (SM r f) (a,c) = ((SM sm' f1), (b, c))
      where
        (sm', b) = f r a
        
secondSM :: SM a b -> SM (c, a) (c, b)
secondSM sm = SM sm f1
  where
    f1 (SM r f) (c,a) = ((SM sm' f1), (c, b))
      where
        (sm', b) = f r a

productSM :: SM a b -> SM c d -> SM (a, c) (b, d)
productSM sm0 sm1 = SM (sm0, sm1) f2
  where
    f2 ((SM r0 f0),(SM r1 f1)) (a, c) = (SM (sm0', sm1') f2, (b, d))
      where
        (sm0', b) = f0 r0 a
        (sm1', d) = f1 r1 c

fanoutSM :: SM a b -> SM a c -> SM a (b, c)
fanoutSM sm0 sm1 = SM (sm0, sm1) f2
  where
    f2 ((SM r0 f0),(SM r1 f1)) a = (SM (sm0', sm1') f2, (b, c))
      where
        (sm0', b) = f0 r0 a
        (sm1', c) = f1 r1 a

-- Evaluation

exec :: SM a b -> [a] -> (SM a b, [b])
exec sm [] = (sm, [])
exec (SM r f) (x:xs) = (sm'', b:bs)
  where 
    (sm', b) = f r x
    (sm'', bs) = (exec sm' xs)
    
-- High order functions
    
execSM :: SM a b -> SM [a] [b]
execSM sm = simpleSM sm exec
