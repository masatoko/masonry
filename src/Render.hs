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

fillRect :: SDL.Renderer -> Rect Double -> IO ()
fillRect r rect =
  SDL.fillRect r $ Just (convRect rect)

drawRect :: SDL.Renderer -> Rect Double -> IO ()
drawRect r rect =
  SDL.drawRect r $ Just (convRect rect)

convRect :: (Num t, RealFrac t, Integral a) => Rect t -> SDL.Rectangle a
convRect (Rect pos size) = SDL.Rectangle pos' size'
  where
    pos' = (round . adjustPos) <$> pos
    size' = (round . adjustSize) <$> size

adjustPos :: Num a => a -> a
adjustPos a = adjustSize a + 100

adjustSize :: Num a => a -> a
adjustSize a = 10 * a
