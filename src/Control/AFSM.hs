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

  SMFunctor(..),
  
  -- * The 'TF' type
  TF(..),
  
  transSM2TF,

  -- * The 'SM' type
  SM(..),
  
  -- * SM Constructors
  newSM,
  simpleSM,
  
  tf, st,

  -- * The 'SMH' type - SM with hidden storage
  SMH(..),

  newSMH,
  simpleSMH,

  hideStorage,

  -- * Source Constructors
  buildSrc,
  simpleSrc,

  -- * Basic State Machines
  constSM,
  idSM,

  delaySM,

  arrSM,

  foldlSM,
  foldlDelaySM,


  -- * Basic SM functions
  composeSM, (>>>>), (<<<<), (^>>>), (>>>^), (^<<<), (<<<^),

  firstSM, secondSM, (****), (&&&&),
  
  leftSM, rightSM, (++++), (||||),
  
  loopSM,
  

  absorb,
  merge,

  -- * High order Machines
  execSM,
  concatSM,

  -- * Evaluation
  step,
  exec


) where

import Control.Arrow
import Control.AFSM.CoreType
import Control.AFSM.Util
import Control.AFSM.Core
import Control.AFSM.Event
import Control.AFSM.TF
import Control.AFSM.SMFunctor
import Control.AFSM.SMH
