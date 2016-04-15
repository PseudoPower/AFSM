-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Arrowized functional state machines.
--
--   This module is inspired by Yampa and the paper
--   /Functional Reactive Programming, Continued*/ written by
--     Henrik Nilsson, Antony Courtney and John Peterson.
-----------------------------------------------------------------------------

module Control.AFSM (
  -- module Control.Category,
  -- module Control.Arrow,
  
  Event(..),
  
  -- SMFunctor(..),
  
  -- * The 'SM' type
  SM(..),
  SMH,

  -- * The 'SMState' type
  --SMState,
  TF(..),
  
  -- * Constructors
  newSM,
  simpleSM,

  -- * Basic State Machines
  constSM,
  idSM,
  arrSM,
  composeSM, (>>>>), (<<<<),
  foldlSM,
  foldlDelaySM,
  delaySM,

  -- * High order functions
  execSM,
  -- hideStorage,
  concatSM,

  -- * Evaluation
  step,
  exec

) where

import Control.Arrow
import Control.AFSM.CoreType
import Control.AFSM.Core
import Control.AFSM.TF
import Control.AFSM.SMFunctor
import Control.AFSM.SMH
