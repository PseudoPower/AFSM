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

import Control.AFSM.CoreType 

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