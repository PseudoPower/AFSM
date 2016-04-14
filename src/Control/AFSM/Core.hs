-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.Core
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.Core where

import Control.Category
import Control.Arrow
import Control.Monad

import Control.AFSM.CoreType

-- Constructors

-- | It is the same with the SM constructor.
newSM :: (SMState s a b) -> s -> SM s a b
newSM = SM

-- | build a simple SM which have only one SMState.
simpleSM :: (s -> a -> (s, b)) -> s -> SM s a b
simpleSM f s = SM f' s
  where
    f' = (\s' a' -> let (s'', b) = f s' a' in (SM f' s'', b))

-- Basic State Machines

-- | build a SM which just output its input
idSM :: SM () a a
idSM = SM (\_ a -> (idSM, a)) ()

-- | build a SM which always return b
constSM :: b -> SM () a b
constSM b = SM f ()
  where
    f _ _ = ((constSM b), b)
    
-- | build a SM from a function
arrSM :: (a -> b) -> SM () a b
arrSM f = SM (\_ a ->(arrSM f, f a)) ()
-- | the same with foldl
foldlSM :: (s -> a -> s) -> s -> SM s a s
foldlSM f s = SM f' s
  where
    f' s' a' = let s'' = f s' a' in (SM f' s'', s'')

-- | the difference from foldlSM is it output the storage first.
foldlDelaySM :: (s -> a -> s) -> s -> SM s a s
foldlDelaySM f s = SM f' s
  where
    f' s' a' = let s'' = f s' a' in (SM f' s'', s')

-- | delay the input with given value.
-- delaySM = foldlDelaySM (const id)
delaySM :: a -> SM a a a
delaySM a = SM f a
  where
    f s' a' = ((SM f a'), s')

-- holdSM :: a -> SM (Event a) a
-- holdSM = undefined

-- filterSM :: (a -> Bool) -> SM a (Event a)
-- filterSM = undefined

-- High order functions

-- | absorb a SM and hide its storage.
absorbRSM :: SM s0 a b -> SM s1 b c -> SM s0 a c
absorbRSM (SM f0 s0) (SM f1 s1) = SM (f2 f0 f1 s1) s0
  where
    f2 f0 f1 s1 s0 a = (SM (f2 f0' f1' s1') s0', c)
      where
        (SM f0' s0', b) = f0 s0 a
        (SM f1' s1', c) = f1 s1 b

absorbLSM :: SM s0 a b -> SM s1 b c -> SM s1 a c
absorbLSM (SM f0 s0) (SM f1 s1) = SM (f2 f0 f1 s0) s1
  where
    f2 f0 f1 s0 s1 a = (SM (f2 f0' f1' s0') s1', c)
      where
        (SM f0' s0', b) = f0 s0 a
        (SM f1' s1', c) = f1 s1 b

-- | absorb a function.
--     absorbR sm f = absorbRSM sm (arrSM f)
--     absorbL f sm = absorbLSM (arrSM f) sm 
absorbR :: SM s a b -> (b -> c) -> SM s a c
absorbR (SM f0 s) f1 = SM (f2 f0) s
  where
    f2 f0 s a = (SM (f2 f0') s', f1 b)
      where
        (SM f0' s', b) = f0 s a
        
absorbL :: (a -> b) -> SM s b c -> SM s a c
absorbL f0 (SM f1 s) = SM (f2 f1) s
  where
    f2 f1 s a = (SM (f2 f1') s', c)
      where
        (SM f1' s', c) = f1 s (f0 a)


-- | compose two SM and merge their storage.
composeSM :: SM s1 b c -> SM s0 a b -> SM (s0, s1) a c
composeSM (SM f1 s1) (SM f0 s0) = SM (f2 f0 f1) (s0, s1)
  where
    f2 f0 f1 (s0, s1) a = (SM (f2 f0' f1') (s0', s1'), c)
      where
        (SM f0' s0', b) = f0 s0 a
        (SM f1' s1', c) = f1 s1 b
        
infixr 1 >>>>, <<<<

-- | Right-to-left composition
(<<<<) :: SM s1 b c -> SM s0 a b -> SM (s0, s1) a c
(<<<<) = composeSM

-- | Left-to-right composition
(>>>>) :: SM s0 a b -> SM s1 b c -> SM (s0, s1) a c
f >>>> g = composeSM g f

-- | converts SM a b -> SM [a] [b], it is very useful to compose SM a [b] and SM b c to SM a [c].
execSM :: SM s a b -> SM s [a] [b]
execSM (SM f s) = SM (f1 f) s
  where
    f1 f s xs = (SM (f1 f') s', bs)
      where
        (SM f' s', bs) = exec (SM f s) xs


joinSM :: Monad m => SM s a (m (m b)) -> SM s a (m b)
joinSM sm = absorbR sm join
        
concatSM :: SM s a [[b]] -> SM s a [b]
concatSM = joinSM

-- eventOutSM :: SM a b -> SM a (Event b)
-- eventOutSM = fmap Event

-- eventSM :: SM a b -> SM (Event a) (Event b)
-- eventSM = undefined

-- slowdownSM :: SM a [b] -> SM a (Event b)
-- slowdownSM = undefined

-- Evaluation

-- | run SM a b with a.
step :: SM s a b -> a -> (SM s a b, b)
step (SM f s) a = f s a

-- | execute SM a b with input [a].
--   Also, it is the map function for SM, perhaps, We should define our own Functor class, the SMFunctor!
exec :: SM s a b -> [a] -> (SM s a b, [b])
exec sm [] = (sm, [])
exec (SM f s) (x:xs) = (sm'', b:bs)
  where
    (sm', b) = f s x
    (sm'', bs) = (exec sm' xs)