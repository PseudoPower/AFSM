-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.ThreadSF.CoreType
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.AFSM.ThreadSF.CoreType (
  writeList2TChan,
  outputTChan,
  tryOutputTChan,
  
  ThreadSF(..),
  idTSF,
  composeTSF,
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


type ThreadSF a b = (TChan a) -> IO ([ThreadId], TChan b)

{-
instance Category ThreadSF where
  id  = idTSF
  (.) = composeTSF
-}

idTSF :: ThreadSF a a
idTSF ta = return ([], ta)

composeTSF :: ThreadSF b c -> ThreadSF a b -> ThreadSF a c
composeTSF tsf1 tsf0 = \ta -> do
  (xs, tb) <- tsf0 ta
  (ys, tc) <- tsf1 tb
  return (xs ++ ys, tc)

