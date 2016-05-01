-----------------------------------------------------------------------------
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- {-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import Control.Monad

import Data.SF
import Data.SF.Draft.IOSF
import Data.Maybe

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.IORef


delayTChan :: Int -> TChan Int -> IO ()
delayTChan x ch = do
  threadDelay 1000000
  atomically $ writeTChan ch x
  delayTChan x ch

in2ret :: SF Int (Maybe Int)
in2ret = simpleSF (\s x -> (s + x, Just (s + x))) 0

main = do
  tc <- newBroadcastTChanIO
  tf <- newThreadSF in2ret
  ret <- tf tc
  forkIO $ outputTChan ret (putStrLn.show)
  forkIO $ delayTChan 1 tc
  forkIO $ delayTChan 10000 tc