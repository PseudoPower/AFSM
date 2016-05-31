-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.SMMonoid
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.AFSM.Draft.SMP where

import Data.Monoid
import Control.Category
import Control.Arrow

import Control.AFSM.CoreType
import Control.AFSM.Core
import Control.AFSM.Draft.SMMonoid

data SMNode 
  = SMId
  | SMArr
  | SMName String
  | SMFst SMNode
  | SMSnd SMNode
  | SMComp SMNode SMNode
  | SMProd SMNode SMNode
  | SMFano SMNode SMNode
  
instance Show SMNode where
  show SMId = "id"
  show SMArr = "arr"
  show (SMName s) = s
  show (SMFst sm) = "(first " ++ show sm ++ ")"
  show (SMSnd sm) = "(second " ++ show sm ++ ")"
  show (SMComp sm0 sm1) = "(" ++ show sm0 ++ "<<<" ++ show sm1 ++ ")"
  show (SMProd sm0 sm1) = "(" ++ show sm0 ++ "***" ++ show sm1 ++ ")"
  show (SMFano sm0 sm1) = "(" ++ show sm0 ++ "&&&" ++ show sm1 ++ ")"
  
  
instance SMMonoid SMNode where
  smempty 1 = SMId
  smempty 2 = SMArr 
  smid 1 node = SMFst node
  smid 2 node = SMSnd node
  smappend 1 n0 n1 = SMComp n0 n1
  smappend 2 n0 n1 = SMProd n0 n1
  smappend 3 n0 n1 = SMFano n0 n1
  
type SMP a b = SM SMNode a b

toSMP :: String -> SM s a b -> SMP a b
toSMP name sm = newSM (f sm) (SMName name)
  where
    f sm node a = (newSM (f sm') node, b)
      where
        (sm', b) = step sm a