-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.SF
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

module Data.AFSM.SF (
  
  SF(..),
  newSF,
  simpleSF,
  
  STF(..),
  transSTF2SF,
  transSF2STF,
  
  buildSrc,
  simpleSrc,
  
  idSF,
  arrSF,
  constSF,
  delaySF,
  foldlSF,
  foldlDelaySF,
  
  Event(..),
  holdSF,
  dropSF,
  filterSF,

  FunctorSF(..),
  (>>>=), (=<<<),
  execSF,
  bindSF,
  
) where

import Control.Category
import Control.Arrow

import Data.AFSM.SF.CoreType
import Data.AFSM.SF.Core
import Data.AFSM.SF.STF
import Data.AFSM.SF.FunctorSF
