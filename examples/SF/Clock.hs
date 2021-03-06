-----------------------------------------------------------------------------
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
-- A simple clock
-----------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Arrow
import Control.Applicative
import Control.Monad

import Data.AFSM

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Concurrent.Timer

import Data.IORef


import Foreign.C.Types
import Foreign.Ptr
import Linear
import Linear.Affine ( Point(P) )

import Data.Word
import Data.StateVar
import Data.Monoid
import Data.Maybe

import Data.Time.Clock

import qualified SDL
import qualified SDL.Time


secondSF :: Int -> SF Int (Int, Bool)
secondSF init = simpleSF (\s x -> let s' = mod (s+x) 60000; y = div s' 1000 in (s', (y, y == 0))) (init * 1000)

minuteSF :: Int -> SF Bool (Int, Bool)
minuteSF = simpleSF (\s x -> if x then let s' = mod (s + 1) 60 in (s', (s', s' == 0)) else (s, (s, False)))

hourSF :: Int -> SF Bool (Int, Bool)
hourSF = simpleSF (\s x -> if x then let s' = mod (s + 1) 12 in (s', (s', s' == 0)) else (s, (s, False)))


clockSF :: (Int, Int, Int) -> SF Int [(Int,Int,Int)]
clockSF (h',m',s') = proc ms -> do
  (s,sb) <- secondSF s' -< ms
  (m,mb) <- minuteSF m' -< sb
  (h,hb) <- hourSF   h' -< mb
  returnA -< [(h,m,s)]

black, white, red, green, blue :: V4 Word8
black = V4 0 0 0 maxBound
white = V4 maxBound maxBound maxBound maxBound
red = V4 maxBound 0 0 maxBound
green = V4 0 maxBound 0 maxBound
blue = V4 0 0 maxBound maxBound

timerCallback :: TChan Int -> Word32 -> IO SDL.Time.RetriggerTimer
timerCallback ch interval = do
  atomically $ writeTChan ch $ fromIntegral interval
  return $ SDL.Time.Reschedule 1000

renderOutput :: SDL.Renderer -> (Int, Int, Int) -> IO ()
renderOutput renderer (h,m,s) = do
  putStrLn $ show (h,m,s)
  SDL.rendererDrawColor renderer $= white
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= black
  SDL.drawRect renderer (Just (SDL.Rectangle (P (V2 50 50)) (V2 200 200)))
  let x = 150; 
      y = 150; 
      a = (1 - fromIntegral s / 30) * pi; 
      xs = x + round (100 * (sin a)); 
      ys = y + round (100 * (cos a));
      b = (1 - fromIntegral m / 30) * pi; 
      xm = x + round (70 * (sin b)); 
      ym = y + round (70 * (cos b));
      c = (1 - fromIntegral h / 6 - fromIntegral m / 360) * pi; 
      xh = x + round (40 * (sin c)); 
      yh = y + round (40 * (cos c));
  SDL.rendererDrawColor renderer $= red
  SDL.drawLine renderer (P (V2 x y)) (P (V2 xs ys))
  SDL.rendererDrawColor renderer $= green
  SDL.drawLine renderer (P (V2 x y)) (P (V2 xm ym))
  SDL.rendererDrawColor renderer $= blue
  SDL.drawLine renderer (P (V2 x y)) (P (V2 xh yh))
  SDL.present renderer    

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Clock" SDL.defaultWindow { SDL.windowInitialSize = V2 300 300 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  time <- getCurrentTime
  putStrLn $ show time
  let t = (floor $ toRational $ utctDayTime time);
      hour = mod ((div t 3600) + 17) 12;
      minute = div (mod t 3600) 60;
      second = mod t 60;
      now = (hour, minute, second);
  
  tc <- newBroadcastTChanIO
  clock <- sf2TSF $ clockSF now
  (tids, ret) <- clock tc
  tid <- forkOS $ outputTChan ret (renderOutput renderer)
  SDL.addTimer 1000 (timerCallback tc)
  
  let loop = (do
      events <- SDL.pollEvents;
      let Any quit = foldMap (\case
              SDL.QuitEvent -> Any True
              SDL.KeyboardEvent e ->
                  if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                      case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                          SDL.ScancodeQ -> Any True
                          _ -> mempty
                     | otherwise -> mempty
              _ -> mempty) $ map SDL.eventPayload events
      unless quit $ loop)
      
  renderOutput renderer now
  SDL.showWindow window
  loop
  
  forM_ tids killThread
  killThread tid
    
  
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit

  


