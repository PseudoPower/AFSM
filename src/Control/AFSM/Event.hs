{-# LANGUAGE GADTs #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.Event where





data Event a = E a | NoE | ErrE String | ExitE deriving (Show, Eq, Ord)