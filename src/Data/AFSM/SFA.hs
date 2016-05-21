-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AFSM.SFA
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
-- {-# LANGUAGE ExistentialQuantification #-}


module Data.AFSM.SFA where

import Prelude hiding ((.))
-- import Data.Coerce

import GHC.Prim

import Control.Category
import Control.Arrow

import Data.AFSM.SF

newtype SFA a b = SFA (SF a [b])

simpleSFA :: (s -> a -> (s, [b])) -> s -> SFA a b
simpleSFA f s = unsafeCoerce# (simpleSF f s)


-- Category instance

instance Category SFA where
  id  = unsafeCoerce# idSFA
  (.) = unsafeCoerce# composeSFA

idSFA :: SF a [a]
idSFA = arr (\x -> [x])

composeSFA :: SF b [c] -> SF a [b] -> SF a [c]
composeSFA sf1 sf0 = (bindSF sf1).sf0


sfaexec' :: SF a [b] -> [a] -> (SF a [b], [b])
sfaexec' sf as = (sf', concat bss)
  where
    (sf', bss) = sfexec sf as

sfaexec :: SFA a b -> [a] -> (SFA a b, [b])
sfaexec = unsafeCoerce# sfaexec'

-- not an instance of Arrow class


-- sfa1 = simpleSFA (\s a -> (s + a, [s, a])) 0
-- res = sfaexec sfa1 [1,2,3]
