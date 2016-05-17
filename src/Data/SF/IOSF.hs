-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.IOSF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Data.SF.IOSF where

import Control.Category
import Control.Arrow
import Control.Monad

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.IORef

import Data.SF.CoreType

{-
data IOSF a b = IOSF {
  refsf :: IORef (SF a (Event b)),
  output :: b -> IO ()
}

runSF :: a -> SF a b -> (SF a b, b)
runSF a (SF sf) = sf a

outputE :: Event b -> (b -> IO ()) -> IO()
outputE NoEvent _ = return ()
outputE (Event b) f = f b

runIOSF :: IOSF a b -> a -> IO ()
runIOSF (IOSF sf o) a = do
  b <- atomicModifyIORef' sf (runSF a)
  outputE b o
-}

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
    Nothing -> putStrLn "oops Nothing"
    Just a -> f a
    
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


type ThreadSF a b = (TChan a) -> IO ([ThreadId], TChan b)

fromTChanSF :: TChanSF a b -> ThreadSF a b
fromTChanSF (TChanSF sf tb) ta = do
  myta <- atomically $ dupTChan ta
  tid <- (forkOS $ forever $ do
    a <- atomically $ readTChan myta
    runIOSF sf a)
  return ([tid], tb)

newThreadSF :: Foldable t => SF a (t b) -> IO (ThreadSF a b)
newThreadSF sf = do
  tsf <- newTChanSF sf
  return $ fromTChanSF tsf
  
idTSF :: ThreadSF a a
idTSF ta = return ([], ta)

composeTSF :: ThreadSF b c -> ThreadSF a b -> ThreadSF a c
composeTSF tsf1 tsf0 = \ta -> do
  (xs, tb) <- tsf0 ta
  (ys, tc) <- tsf1 tb
  return (xs ++ ys, tc)
  
