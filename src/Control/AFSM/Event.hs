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

module Control.AFSM.Event where

-- | 'Event' type, there are 4 different events: event a, no event, error event string and exit event.
-- data Event a = Event a | NoEvent | ErrEvent String | ExitEvent deriving (Show, Eq, Ord)