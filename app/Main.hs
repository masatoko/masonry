{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import Linear.Affine
import Linear.V2
import Linear.V4
import System.Random

import SDL (($=))
import qualified SDL

import Type
import Masonry (test)
import Conf

sizeW :: Int
sizeW = 40

sizeH :: Int
sizeH = 40

size = V2 (fromIntegral sizeW) (fromIntegral sizeH)

main :: IO ()
main = do
  (pathConf:arg:_) <- getArgs
  conf <- importConf pathConf
  print conf

  SDL.initializeAll

  win <- SDL.createWindow "Masonry" SDL.defaultWindow {SDL.windowInitialSize = V2 500 500}
  SDL.showWindow win

  generate conf arg win
  _ <- getChar

  SDL.destroyWindow win
  SDL.quit

generate :: Conf -> String -> SDL.Window -> IO ()
generate conf arg win = do
  r <- SDL.createRenderer win 0 SDL.defaultRenderer
  SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend
  SDL.clear r
  --
  let i = read arg
  forM_ [0..] $ \x -> do
    test conf r $ i + x
    threadDelay 1000000
  --
  SDL.present r
