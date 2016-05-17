{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Demo (main) where

import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Monad

import Foreign.C.Types
import Linear
import Linear.Affine ( Point(P) )

import Data.Word
import Data.StateVar
import Data.Monoid
import Data.Maybe

import qualified SDL

white, red, green, blue :: V4 Word8
white = V4 maxBound maxBound maxBound maxBound
red = V4 maxBound 0 0 maxBound
green = V4 0 maxBound 0 maxBound
blue = V4 0 0 maxBound maxBound

colors = [red, green, blue]

main :: IO ()
main = do
  SDL.initializeAll
  
  window <- SDL.createWindow "Demo" SDL.defaultWindow { SDL.windowInitialSize = V2 800 600 }
  
  SDL.showWindow window
  
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  let loop x = do
      events <- SDL.pollEvents
      if events /= [] then putStrLn $ show events else return ()
      let (Any quit, Last newX) = foldMap (\case
              SDL.QuitEvent -> (Any True, mempty)
              SDL.KeyboardEvent e ->
                  if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                      case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                          SDL.Scancode1 -> (Any False, Last (Just 0))
                          SDL.Scancode2 -> (Any False, Last (Just 1))
                          SDL.Scancode3 -> (Any False, Last (Just 2))
                          SDL.ScancodeQ -> (Any True, mempty)
                          _ -> mempty
                     | otherwise -> mempty
              _ -> mempty) $
              map SDL.eventPayload events
          x' = newX <|> x
              
      SDL.rendererDrawColor renderer $= white
      SDL.clear renderer
      SDL.rendererDrawColor renderer $= colors!!(fromJust x)
      SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 100 100)) (V2 100 200)))
      SDL.present renderer
    
      
      unless quit $ loop x'
    
  loop $ Just 0
  

  

  SDL.destroyWindow window
  SDL.quit
