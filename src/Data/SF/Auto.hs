-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SF.Auto
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- {-# LANGUAGE ExistentialQuantification #-}

module Data.SF.Auto where

import Control.Category
import Control.Arrow
import Control.Monad

-- | Control.Arrow.Transformer.Automaton
-- data Auto f a b = forall f. (Arrow f) => Auto (f a (Auto f a b, b))
data Auto f a b = Auto (f a (Auto f a b, b))
