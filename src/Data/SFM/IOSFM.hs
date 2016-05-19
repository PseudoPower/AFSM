-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.Draft.IOSF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Data.SF.IOSFM where

import Control.Category
import Control.Arrow
import Control.Monad hiding (forM_)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Data.IORef

import Data.Foldable hiding (sequence_)

import Data.SF.SFM

writeList2TChan :: TChan a -> [a] -> IO ()
writeList2TChan ch ls = sequence_ (map (\x -> atomically $ writeTChan ch x) ls)

outputTChan :: TChan a -> (a -> IO ()) -> IO ()
outputTChan ch f = do
  mych <- atomically $ dupTChan ch
  forever $ do
    a <- atomically $ readTChan mych
    f a
    
    
data IOSFM a b = forall t. Foldable t => IOSFM (IORef (SFM IO a (t b))) (b -> IO ())

runIOSFM :: IOSFM a b -> a -> IO ()
runIOSFM (IOSFM ref o) a = do
  (SFM sf) <- readIORef ref
  (sfm', b) <- sf a
  forM_ b o
  writeIORef ref sfm'
  

data TChanSFM a b = TChanSFM (IOSFM a b) (TChan b)

newTChanSFM :: Foldable t => SFM IO a (t b) -> IO (TChanSFM a b)
newTChanSFM sf = do
  o <- newBroadcastTChanIO
  s <- newIORef sf
  return $ TChanSFM (IOSFM s (\b -> atomically $ writeTChan o b)) o


type ThreadSF a b = (TChan a) -> IO (TChan b)

fromTChanSFM :: TChanSFM a b -> ThreadSF a b
fromTChanSFM (TChanSFM sf tb) ta = do
  myta <- atomically $ dupTChan ta
  (forkIO $ forever $ do
    a <- atomically $ readTChan myta
    runIOSFM sf a)
  return tb

sfm2ThreadSF :: Foldable t => SFM IO a (t b) -> IO (ThreadSF a b)
sfm2ThreadSF sf = do
  tsf <- newTChanSFM sf
  return $ fromTChanSFM tsf
