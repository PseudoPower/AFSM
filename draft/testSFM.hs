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
import Data.SF.SFM
import Data.Maybe
import Data.Time.Clock



timerSFM :: a -> Int -> IO (SFM IO () (Maybe a))
timerSFM a n = do
  t <- getCurrentTime
  simpleSFM f t
    where
      f t () = do
        t' <- getCurrentTime
        if truncate (diffUTCTime t' t) < n then return (t, Nothing)
        else return (t', Just a)
        
test =  (timerSFM 1 10) >>= simpleSrcM
      
main = do
  test >>= (\xs -> forM_ xs (putStrLn.show))
  return ()
