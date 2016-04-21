-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.SF.CoreType
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.SF.CoreType where

data SF a b = SF (a -> (SF a b, b))

-- SFTF doesn't work.
-- data SFTF s a b = SFT (s -> a -> (SF a b, b))