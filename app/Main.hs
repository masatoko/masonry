{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import qualified SDL
import Linear.V2

import Masonry (test)

main :: IO ()
main = do
  -- test 111

  SDL.initializeAll

  win <- SDL.createWindow "Masonry" SDL.defaultWindow {SDL.windowInitialSize = V2 800 800}
  SDL.showWindow win

  threadDelay 1000000

  SDL.destroyWindow win
  SDL.quit
