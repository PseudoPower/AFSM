-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.Auto
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- {-# LANGUAGE ExistentialQuantification #-}

module Data.AFSM.Auto where

import Control.Category
import Control.Arrow
import Control.Monad

-- | Control.Arrow.Transformer.Automaton
-- data Auto f a b = forall f. (Arrow f) => Auto (f a (Auto f a b, b))
-- data Auto f a b = Auto (f a (Auto f a b, b))

class Auto z where
  build :: (a -> (z a b, b)) -> z a b
  step :: (z a b) -> a -> (z a b, b)
  
instance Auto z => Category z where
  id = idAuto
  (.) = composeAuto
  
idAuto :: Auto z => z a a
idAuto = build (\a -> (idAuto, a))

composeAuto :: Auto z => z b c -> z a b -> z a c
composeAuto zbc zab = build f
  where
    f a = (composeAuto zab', zbc', c)
      where
        (zab', b) = step zab a
        (zbc', c) = step zbc b
