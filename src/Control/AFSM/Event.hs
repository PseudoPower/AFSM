-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.Event
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.Event (
  extractEvents
)where

import Control.Applicative
import Control.Monad

import Control.AFSM.CoreType
import Control.AFSM.SMFunctor

-- | 'Event' type, there are 4 different events: event a, no event, error event string and exit event.
-- data Event a = Event a | NoEvent | ErrEvent String | ExitEvent deriving (Show, Eq, Ord)

extractEvents :: [Event a] -> [a]
extractEvents [] = []
extractEvents (x:xs) = case x of
  Event a -> a:ys
  NoEvent -> ys
  ErrEvent s -> []
  ExitEvent -> []
  where
    ys = (extractEvents xs)
    
instance Functor Event where
  fmap f (Event a) = Event (f a)
  fmap _ NoEvent = NoEvent
  fmap _ (ErrEvent s) = (ErrEvent s)
  fmap _  ExitEvent = ExitEvent
  
instance Applicative Event where
  pure a = Event a
  (<*>) (Event f) m = fmap f m
  (<*>) (ErrEvent s0) (ErrEvent s1) = ErrEvent $ s0 ++ "," ++ s1
  (<*>) (ErrEvent s0) _ = ErrEvent s0
  (<*>) ExitEvent _ = ExitEvent
  (<*>) NoEvent _ = NoEvent

instance Monad Event where
  return = pure
  (>>=) (Event a) f = f a
  (>>=) NoEvent _ = NoEvent
  (>>=) (ErrEvent s) _ = (ErrEvent s)
  (>>=) ExitEvent _ = ExitEvent
  

instance SMFunctor Event where
  smexec sm NoEvent = (sm, NoEvent)
  smexec sm (ErrEvent s) = (sm, ErrEvent s)
  smexec sm ExitEvent = (sm, ExitEvent)
  smexec (SM (TF f) s) (Event a) = (sm', Event b)
    where (sm', b) = f s a
