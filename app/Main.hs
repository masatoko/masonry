{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Linear.Affine
import Linear.V2
import Linear.V4
import System.Random

import SDL (($=))
import qualified SDL

import Type
import Masonry (test)
import PrimRoom (makePrimRoom)
import Render (drawRect)

sizeW :: Int
sizeW = 40

sizeH :: Int
sizeH = 40

size = V2 (fromIntegral sizeW) (fromIntegral sizeH)

main :: IO ()
main = do
  -- test 111

  SDL.initializeAll

  win <- SDL.createWindow "Masonry" SDL.defaultWindow {SDL.windowInitialSize = V2 600 600}
  SDL.showWindow win

  generate win
  threadDelay 1000000

  SDL.destroyWindow win
  SDL.quit

generate :: SDL.Window -> IO ()
generate win = do
  r <- SDL.createRenderer win 0 SDL.defaultRenderer
  SDL.clear r
  --
  let room = fst . makePrimRoom size $ mkStdGen 100
  print room
  SDL.rendererDrawColor r $= V4 0 0 255 255
  drawRect r room
  --
  SDL.present r
