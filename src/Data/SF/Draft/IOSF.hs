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

module Data.SF.Draft.IOSF where

import Control.Category
import Control.Arrow

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.IORef

import Data.SF.CoreType

data IOSF a b = IOSF {
  refsf :: IORef (SF a (Event b)),
  output :: b -> IO ()
}

-- data TChanSF a b = TChanSF (IOSF a b) (TChan b)

newTChanSF :: SF a (Event b) -> IO (IOSF a b, TChan b)
newTChanSF sf = do
  o <- newBroadcastTChanIO
  s <- newIORef sf
  return (IOSF s (\b -> atomically $ writeTChan o b), o)

runSF :: a -> SF a b -> (SF a b, b)
runSF a (SF sf) = sf a

outputE :: Event b -> (b -> IO ()) -> IO()
outputE NoEvent _ = return ()
outputE (Event b) f = f b

runIOSF :: IOSF a b -> IO a -> IO ()
runIOSF (IOSF sf o) ia = do
  a <- ia
  b <- atomicModifyIORef' sf (runSF a)
  outputE b o
