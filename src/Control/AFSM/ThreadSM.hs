-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.ThreadSM
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- {-# LANGUAGE TypeSynonymInstances #-}

module Control.AFSM.ThreadSM (
  writeList2TChan,
  outputTChan,
  tryOutputTChan,
  ThreadSM(..),
  idTSM,
  composeTSM,
) where

import Control.Category
import Control.Arrow
import Control.Monad

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.IORef


writeList2TChan :: TChan a -> [a] -> IO ()
writeList2TChan ch ls = sequence_ (map (\x -> atomically $ writeTChan ch x) ls)

outputTChan :: TChan a -> (a -> IO ()) -> IO ()
outputTChan ch f = do
  mych <- atomically $ dupTChan ch
  forever $ do
    a <- atomically $ readTChan mych
    f a
    
tryOutputTChan :: TChan a -> (a -> IO ()) -> IO ()
tryOutputTChan ch f = do
  mych <- atomically $ dupTChan ch
  ret <- atomically $ tryReadTChan mych
  case ret of
    Nothing -> return ()
    Just a -> f a


type ThreadSM a b = (TChan a) -> IO ([ThreadId], TChan b)

{-
instance Category ThreadSM where
  id  = idTSM
  (.) = composeTSM
-}

idTSM :: ThreadSM a a
idTSM ta = return ([], ta)

composeTSM :: ThreadSM b c -> ThreadSM a b -> ThreadSM a c
composeTSM tsm1 tsm0 = \ta -> do
  (xs, tb) <- tsm0 ta
  (ys, tc) <- tsm1 tb
  return (xs ++ ys, tc)
