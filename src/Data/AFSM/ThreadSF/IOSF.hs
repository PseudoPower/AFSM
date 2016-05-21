-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.ThreadSF.IOSF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Data.AFSM.ThreadSF.IOSF (
  sf2TSF,
) where

import Control.Category
import Control.Arrow
import Control.Monad

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.IORef

import Data.AFSM.SF
import Data.AFSM.ThreadSF.CoreType
    
data IOSF a b = forall t. Foldable t => IOSF (IORef (SF a (t b))) (b -> IO ())

runSF :: a -> SF a b -> (SF a b, b)
runSF a (SF sf) = sf a

runIOSF :: IOSF a b -> a -> IO ()
runIOSF (IOSF sf o) a = do
  b <- atomicModifyIORef' sf (runSF a)
  forM_ b o
  
runIOSFwithTChan :: IOSF a b -> TChan a -> IO ()
runIOSFwithTChan sf ta = do
  myta <- atomically $ dupTChan ta
  (forever $ do
    a <- atomically $ readTChan myta
    runIOSF sf a)


data TChanSF a b = TChanSF (IOSF a b) (TChan b)

newTChanSF :: Foldable t => SF a (t b) -> IO (TChanSF a b)
newTChanSF sf = do
  o <- newBroadcastTChanIO
  s <- newIORef sf
  return $ TChanSF (IOSF s (\b -> atomically $ writeTChan o b)) o

fromTChanSF :: TChanSF a b -> ThreadSF a b
fromTChanSF (TChanSF sf tb) ta = do
  myta <- atomically $ dupTChan ta
  tid <- (forkOS $ forever $ do
    a <- atomically $ readTChan myta
    runIOSF sf a)
  return ([tid], tb)

sf2TSF :: Foldable t => SF a (t b) -> IO (ThreadSF a b)
sf2TSF sf = do
  tsf <- newTChanSF sf
  return $ fromTChanSF tsf

  
