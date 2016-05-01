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

{-
psMV = newMVar ()

safePutStrLn :: String -> IO()
safePutStrLn s = do
  m <- psMV
  takeMVar m
  putStrLn s
  putMVar m ()
-}

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
  
  tc1 <- newBroadcastTChanIO
  tf1 <- newThreadSF in2ret
  ret1 <- tf1 tc1
    
  forkIO $ outputTChan ret (putStrLn.show)
  forkIO $ outputTChan ret1 (putStrLn.show)
  forkIO $ delayTChan 1 tc
  forkIO $ delayTChan 10000 tc1
  getContents >>= putStrLn
