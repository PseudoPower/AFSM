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
import Control.AFSM.Util

infixr 3 ****
infixr 3 &&&&
infixr 2 ++++
infixr 2 ||||
infixr 1 >>>>, <<<<
infixr 1 ^>>>, >>>^
infixr 1 ^<<<, <<<^


-- | Source
--   There are two kinds of source. 
--   First one is using the output of `SM s a a` as its input, then it becomes a perpetual motion, :)  
--   Second one is a SM which ignore its input, and output something based on its storage.
--   The second one is easier to understand and use.

-- | build a source, for example:
--   buildSrc $ foldlDelaySM (const (+1)) 0
--     [0..]
--   buildSrc $ foldlDelaySM (+) 1
--     [1, 2, 4, 8, ...]
buildSrc :: SM s a a -> [a]
buildSrc sm = a:(buildSrc sm')
  where
    (sm', a) = step sm a

-- | build a simple source, which ignore the inputs
--     fibsSM :: SM (Int, Int) () Int
--     fibsSM = simpleSM (\(a, b) () -> ((b, a+b), a)) (0, 1)
--     take 10 $ simpleSrc fibsSM
--       [0,1,1,2,3, ...]
simpleSrc :: SM s () a -> [a]
simpleSrc sm = a:(simpleSrc sm')
  where
    (sm', a) = step sm ()

-- Basic State Machines
        
-- | build a SM which just output its input
idSM :: SM () a a
idSM = newSM (\_ a -> (idSM, a)) ()

-- | build a SM which always return b
constSM :: b -> SM () a b
constSM b = newSM f ()
  where
    f _ _ = ((constSM b), b)

-- | delay the input with given value.
-- delaySM = foldlDelaySM (const id)
delaySM :: a -> SM a a a
delaySM a = newSM f a
  where
    f s' a' = ((newSM f a'), s')

-- | build a SM from a function
arrSM :: (a -> b) -> SM () a b
arrSM f = newSM (\_ a ->(arrSM f, f a)) ()

-- | the same with foldl
foldlSM :: (s -> a -> s) -> s -> SM s a s
foldlSM f s = newSM f' s
  where
    f' s' a' = (newSM f' s'', s'')
      where
        s'' = f s' a'

-- | the difference from foldlSM is it output the storage first.
foldlDelaySM :: (s -> a -> s) -> s -> SM s a s
foldlDelaySM f s = newSM f' s
  where
    f' s' a' = (newSM f' s'', s')
      where
        s'' = f s' a'

-- holdSM :: a -> SM (Event a) a
-- holdSM = undefined

-- filterSM :: (a -> Bool) -> SM a (Event a)
-- filterSM = undefined


-- High order functions

-- | absorb a function.
--     absorbR sm f = absorbRSM sm (arrSM f)
--     absorbL f sm = absorbLSM (arrSM f) sm
absorbR :: SM s a b -> (b -> c) -> SM s a c
absorbR (SM (TF f0) s) f1 = newSM (f2 f0) s
  where
    f2 f0 s a = (newSM (f2 f0') s', f1 b)
      where
        (SM (TF f0') s', b) = f0 s a

absorbL :: (a -> b) -> SM s b c -> SM s a c
absorbL f0 (SM (TF f1) s) = newSM (f2 f1) s
  where
    f2 f1 s a = (newSM (f2 f1') s', c)
      where
        (SM (TF f1') s', c) = f1 s (f0 a)

(^>>>) = absorbL
(>>>^) = absorbR
(<<<^) = flip absorbL
(^<<<) = flip absorbR

-- Category instance

-- idSM

-- | compose two SM and merge their storage.
composeSM :: SM s1 b c -> SM s0 a b -> SM (s0, s1) a c
composeSM (SM (TF f1) s1) (SM (TF f0) s0) = newSM (f2 f0 f1) (s0, s1)
  where
    f2 f0 f1 (s0, s1) a = (newSM (f2 f0' f1') (s0', s1'), c)
      where
        (SM (TF f0') s0', b) = f0 s0 a
        (SM (TF f1') s1', c) = f1 s1 b

-- | Right-to-left composition
(<<<<) :: SM s1 b c -> SM s0 a b -> SM (s0, s1) a c
(<<<<) = composeSM

-- | Left-to-right composition
(>>>>) :: SM s0 a b -> SM s1 b c -> SM (s0, s1) a c
f >>>> g = composeSM g f


-- Arrow instance

-- arrSM


firstSM :: SM s a b -> SM s (a, c) (b, c)
firstSM (SM (TF f) s) = newSM (f1 f) s
  where
    f1 f s (a, c) = (newSM (f1 f') s', (b, c))
      where
        (SM (TF f') s', b) = f s a

secondSM :: SM s a b -> SM s (c, a) (c, b)
secondSM (SM (TF f) s) = newSM (f1 f) s
  where
    f1 f s (c, a) = (newSM (f1 f') s', (c, b))
      where
        (SM (TF f') s', b) = f s a

productSM :: SM s0 a b -> SM s1 c d -> SM (s0, s1) (a, c) (b, d)
productSM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1) (s0, s1)
  where
    f2 f0 f1 (s0, s1) (a, c) = (newSM (f2 f0' f1') (s0', s1'), (b, d))
      where
        (SM (TF f0') s0', b) = f0 s0 a
        (SM (TF f1') s1', d) = f1 s1 c

fanoutSM :: SM s0 a b -> SM s1 a c -> SM (s0, s1) a (b, c)
fanoutSM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1) (s0, s1)
  where
    f2 f0 f1 (s0, s1) a = (newSM (f2 f0' f1') (s0', s1'), (b, c))
      where
        (SM (TF f0') s0', b) = f0 s0 a
        (SM (TF f1') s1', c) = f1 s1 a

(****) = productSM

(&&&&) = fanoutSM

{-
firstSM :: SM s a b -> SM s (a, c) (b, c)
firstSM sm = absorb (\(a, c) -> a) (\(a, c) b -> (b, c)) sm

secondSM :: SM s a b -> SM s (c, a) (c, b)
secondSM sm = absorb (\(c, a) -> a) (\(c, a) b -> (c, b)) sm

(****) :: SM s0 a b -> SM s1 c d -> SM (s0, s1) (a, c) (b, d)
(****) sm0 sm1 = merge (\(a, c) -> (a, c)) (\a b d -> (b, d)) sm0 sm1

(&&&&) :: SM s0 a b -> SM s1 a c -> SM (s0, s1) a (b, c)
(&&&&) sm0 sm1 = merge (\a -> (a, a)) (\a b0 b1 -> (b0, b1)) sm0 sm1
-}

-- ArrowChoice instance

leftSM :: SM s a b -> SM s (Either a c) (Either b c)
leftSM (SM (TF f0) s) = newSM (f1 f0) s
  where
    f1 f0 s (Right c) = (newSM (f1 f0) s, Right c)
    f1 f0 s (Left a) = (newSM (f1 f0') s', Left b)
      where
        (SM (TF f0') s', b) = f0 s a

rightSM :: SM s a b -> SM s (Either c a) (Either c b)
rightSM (SM (TF f0) s) = newSM (f1 f0) s
  where
    f1 f0 s (Left c) = (newSM (f1 f0) s, Left c)
    f1 f0 s (Right a) = (newSM (f1 f0') s', Right b)
      where
        (SM (TF f0') s', b) = f0 s a

sumSM :: SM s0 a b -> SM s1 c d -> SM (s0,s1) (Either a c) (Either b d)
sumSM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1) (s0, s1)
  where
    f2 f0 f1 (s0, s1) (Left a)  = let (SM (TF f0') s0', b) = f0 s0 a in (newSM (f2 f0' f1) (s0', s1), Left b)
    f2 f0 f1 (s0, s1) (Right c) = let (SM (TF f1') s1', d) = f1 s1 c in (newSM (f2 f0 f1') (s0, s1'), Right d)

faninSM :: SM s0 a c -> SM s1 b c -> SM (s0, s1) (Either a b) c
faninSM (SM (TF f0) s0) (SM (TF f1) s1) = newSM (f2 f0 f1) (s0, s1)
  where
    f2 f0 f1 (s0, s1) (Left a)  = let (SM (TF f0') s0', c) = f0 s0 a in (newSM (f2 f0' f1) (s0', s1), c)
    f2 f0 f1 (s0, s1) (Right b) = let (SM (TF f1') s1', c) = f1 s1 b in (newSM (f2 f0 f1') (s0, s1'), c)

    
(++++) = sumSM

(||||) = faninSM

-- ArrowLoop

loopSM :: SM s (a, c) (b, c) -> SM s a b
loopSM (SM (TF f0) s) = newSM (f1 f0) s
  where
    f1 f0 s a = (newSM (f1 f0') s', b)
      where
        (SM (TF f0') s', (b, c)) = f0 s (a, c)




-- | converts SM a b -> SM [a] [b], it is very useful to compose SM a [b] and SM b c to SM a [c].
execSM :: SM s a b -> SM s [a] [b]
execSM (SM (TF f) s) = newSM (f1 f) s
  where
    f1 f s xs = (newSM (f1 f') s', bs)
      where
        (SM (TF f') s', bs) = exec (newSM f s) xs


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
step (SM (TF f) s) a = f s a

-- | execute SM a b with input [a].
--   Also, it is the map function for SM, perhaps, We should define our own Functor class, the SMFunctor!
exec :: SM s a b -> [a] -> (SM s a b, [b])
exec sm [] = (sm, [])
exec (SM (TF f) s) (x:xs) = (sm'', b:bs)
  where
    (sm', b) = f s x
    (sm'', bs) = (exec sm' xs)

-- Functor

instance Functor (SM s a) where
  fmap = fmapSM

-- | fmapSM f sm = sm >>> arr f
fmapSM :: (b -> c) -> SM s a b -> SM s a c
fmapSM f sm = absorbR sm f
