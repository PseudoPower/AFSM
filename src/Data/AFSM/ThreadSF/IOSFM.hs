-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.ThreadSF.IOSFM
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Data.AFSM.ThreadSF.IOSFM (
  sfm2TSF,
) where

import Control.Category
import Control.Arrow
import Control.Monad hiding (forM_)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Data.IORef

import Data.Foldable hiding (sequence_)

import Data.AFSM.SFM
import Data.AFSM.ThreadSF.CoreType
    
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

fromTChanSFM :: TChanSFM a b -> ThreadSF a b
fromTChanSFM (TChanSFM sf tb) ta = do
  myta <- atomically $ dupTChan ta
  tid <- (forkIO $ forever $ do
    a <- atomically $ readTChan myta
    runIOSFM sf a)
  return ([tid], tb)

sfm2TSF :: Foldable t => SFM IO a (t b) -> IO (ThreadSF a b)
sfm2TSF sf = do
  tsf <- newTChanSFM sf
  return $ fromTChanSFM tsf
