{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
-- import Control.Concurrent (threadDelay)
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
  (arg:_) <- getArgs
  SDL.initializeAll

  win <- SDL.createWindow "Masonry" SDL.defaultWindow {SDL.windowInitialSize = V2 1000 1000}
  SDL.showWindow win

  generate arg win
  _ <- getChar

  SDL.destroyWindow win
  SDL.quit

generate :: String -> SDL.Window -> IO ()
generate arg win = do
  r <- SDL.createRenderer win 0 SDL.defaultRenderer
  SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend
  SDL.clear r
  --
  test r $ read arg
  --
  SDL.present r
