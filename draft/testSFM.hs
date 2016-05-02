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


sfm0 = idSFM :: SFM Maybe a a

main = do
  return ()
