-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.SFM
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Stateful functions with Monad
--
-----------------------------------------------------------------------------

module Data.AFSM.SFM (
  SFM(..),
  newSFM,
  simpleSFM,

) where

import Control.Category
import Control.Arrow

import Data.AFSM.SFM.CoreType
import Data.AFSM.SFM.Core
-- import Data.SFM.STFM
-- import Data.SFM.FunctorSFM
