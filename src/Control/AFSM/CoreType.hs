-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.CoreType
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- {-# LANGUAGE ExistentialQuantification #-}

module Control.AFSM.CoreType where

-- | 'TF' is a type representing a transition function.
--     s: storage, a: input, b: output
--   Let's explain more about 'TF'. When a state gets an input a, 
--   it should do three things base on the storage and input: 
--     find the next state, update storage and output b.
--   That's why it looks like this:
--     (storage -> a -> (SM newState newStorage, b))
--     type TF storage input output = (storage -> input -> (SM storage input output, output))
newtype TF s a b = TF (s -> a -> (SM s a b, b))

type STF s a b = (s -> a -> (s, b))

-- | 'SM' is a type representing a state machine.
--     (TF s a b): initial state(transition function), s: initial storage
--     SM storage input output = SM (TF storage input output) storage
data SM s a b = SM (TF s a b) s

tf :: SM s a b -> (s -> a -> (SM s a b, b))
tf (SM (TF f) _) = f

st :: SM s a b -> s
st (SM _ s) = s

-- Constructors

-- | It is the same with the SM constructor.
newSM :: (s -> a -> (SM s a b, b)) -> s -> SM s a b
newSM tf s = SM (TF tf) s

-- | build a simple SM which have only one TF.
simpleSM :: (STF s a b) -> s -> SM s a b
simpleSM f s = newSM f' s
  where
    f' s' a' = (newSM f' s'', b)
      where
        (s'', b) = f s' a'

-- | build a SM which can choose STF based on the input        
choiceSM :: (a -> STF s a b) -> s -> SM s a b
choiceSM cf s = simpleSM f s
  where
    f s a = (s', b)
      where 
        (s', b) = (cf a) s a

instance (Show s) => Show (SM s a b) where
  show (SM f s) = show s

-- | 'SMH' is the type of the state machine with hidden or no storage.
--   It is the same type with 
--     Circuit a b = Circuit (a -> Circuit a b, b)
type SMH a b = SM () a b

-- | 'Event' type, there are 4 different events: event a, no event, error event string and exit event.
data Event a = Event a | NoEvent | ErrEvent String | ExitEvent deriving (Show, Eq, Ord)


-- (SM (TF f0) s0) (SM (TF f1) s1)