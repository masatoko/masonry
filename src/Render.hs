module Render where

import Data.Word (Word8)
import Linear.V4

import SDL (($=))
import qualified SDL

import Type

clearScreen :: SDL.Renderer -> V4 Word8 -> IO ()
clearScreen r color = do
  SDL.rendererDrawColor r $= color
  SDL.clear r

drawRect :: SDL.Renderer -> Rect Double -> IO ()
drawRect r rect =
  SDL.drawRect r (Just (convRect rect))

convRect (Rect pos size) = SDL.Rectangle pos' size'
  where
    pos' = (round . adjust) <$> pos
    size' = (round . adjust) <$> size

adjust a = 10 * a
