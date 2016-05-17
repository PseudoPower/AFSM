-----------------------------------------------------------------------------
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
-- Elevator
-----------------------------------------------------------------------------


module Elevator where

import Control.AFSM
import Control.AFSM.SMH

import Data.Maybe

elevatorIdle :: Int -> SM (Bool, Int, [Int], [Int]) Int Int
elevatorIdle floor = newSM (\s a -> (elevatorIdle floor, floor)) (True, floor, [], [])

elevatorIdleC :: SM (Bool, Int, [Int], [Int]) Int Int 
elevatorIdleC = newSM (\s a -> (elevatorIdleC, a)) (False, 0, [], [])



elevatorGoup :: Int -> Int -> SM (Bool, Int, [Int], [Int]) Int Int ->SM (Bool, Int, [Int], [Int]) Int Int
elevatorGoup x y f =  ((tf f)  )

elevatorGoDown ::  Int -> Int -> SM (Bool, Int, [Int], [Int]) Int Int ->SM (Bool, Int, [Int], [Int]) Int Int
elevatorGodown = undefined
-}
