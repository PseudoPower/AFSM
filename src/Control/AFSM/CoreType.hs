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

-- | 'SMState' is a type representing a transition function.
--     s: storage, a: input, b: output
--   Let's explain more about 'SMState'. When a state gets an input a, 
--   it should do three things base on the storage and input: 
--     find the next state, update storage and output b.
--   That's why it looks like this:
--     (storage -> a -> (SM newState newStorage, b))
--     type SMState storage input output = (storage -> input -> (SM storage input output, output))
type SMState s a b = (s -> a -> (SM s a b, b))

-- | 'SM' is a type representing a state machine.
--     (SMState s a b): initial state(transition function), s: initial storage
--     SM storage input output = SM (SMState storage input output) storage
data SM s a b = SM { tf :: (SMState s a b), st :: s }

instance (Show s) => Show (SM s a b) where
  show (SM f s) = show s

-- | 'SMH' is the type of the state machine with hidden or no storage.
--   It is the same type with 
--     Circuit a b = Circuit (a -> Circuit a b, b)
type SMH a b = SM () a b

-- | 'Event' type, there are 4 different events: event a, no event, error event string and exit event.
data Event a = Event a | NoEvent | ErrEvent String | ExitEvent deriving (Show, Eq, Ord)