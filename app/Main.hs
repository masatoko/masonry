{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (exitSuccess)
import System.Environment (getArgs)
import Control.Monad (when, unless, forM_)
import Control.Concurrent (threadDelay)
import Linear.Affine
import Linear.V2
import Linear.V4
import System.Random
import Data.List (find)
import Data.Maybe (isJust)

import SDL (($=))
import qualified SDL

import Type
import Masonry (generate)
import Conf

main :: IO ()
main = do
  (pathConf:arg:_) <- getArgs
  conf <- importConf pathConf
  print conf

  SDL.initializeAll

  let w = fromIntegral $ 10 * confWidth conf + 200
      h = fromIntegral $ 10 * confHeight conf + 200
  win <- SDL.createWindow "Masonry" SDL.defaultWindow {SDL.windowInitialSize = V2 w h}
  SDL.showWindow win

  go conf arg win

  SDL.destroyWindow win
  SDL.quit

  where
    go :: Conf -> String -> SDL.Window -> IO ()
    go conf arg win = do
      r <- SDL.createRenderer win 0 SDL.defaultRenderer
      SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend
      SDL.clear r
      --
      let i = read arg
      let loop x = do
            generate conf r $ i + x
            --
            putStrLn "---------------------------------------------------------"
            events <- SDL.pollEvents
            quit <- if confWaitKey conf
                      then do
                        putStrLn "Waiting for key input"
                        waitKey
                      else
                        return . elem SDL.QuitEvent $ map SDL.eventPayload events
            unless quit $ loop (x + 1)

      loop 0
      --
      SDL.present r

waitKey :: IO Bool
waitKey = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
      anykey = isJust . find isKeyEvent $ map SDL.eventPayload events
  if quit || anykey
    then do
      putStrLn "Next ..."
      return quit
    else waitKey
  where
    isKeyEvent (SDL.KeyboardEvent _) = True
    isKeyEvent _                     = False
