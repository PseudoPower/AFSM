-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Stateful functions
--   It is the same with SMH, and it just removes the empty storage.
-----------------------------------------------------------------------------

module Data.SF (
  
  SF(..),
  newSF,
  simpleSF,
  
  STF(..),
  transSTF2SF,
  transSF2STF,

  SFunctor(..),
  
) where

import Control.Category
import Control.Arrow

import Data.SF.CoreType
import Data.SF.Core
import Data.SF.STF
import Data.SF.SFunctor