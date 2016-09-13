module Masonry
( test
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Linear.Affine
import Linear.V2
import Linear.V4
import Linear.Vector
import System.Random
import Data.List (scanl')

import SDL (($=))
import qualified SDL

import Type (Rect (..), fat)
import PrimRoom (makePrimRoom)
import qualified SVG
import Separate.Physics (separateRooms)

import Render

test :: SDL.Renderer -> Int -> IO ()
test rnd seed = do
  let drawRss rss =
        forM_ (zip [0..] rss) $ \(i,rs) -> do
          when (i `mod` 10 == 0) $ print i
          clearScreen rnd $ V4 0 0 0 255
          frame
          --
          mapM_ (draw blue rnd) rs
          SDL.present rnd
          --
          threadDelay 5000

  let rss0 = separateRooms size 200 0.1 rs0
      rss1 = separateRooms size 500 0     $ last rss0
  drawRss rss0
  drawRss rss1

  let rs' = map roundPos $ removeOutRooms size (last rss1)
  SDL.rendererDrawColor rnd $= V4 0 0 0 200
  drawRect rnd $ Rect (pure 0) size
  frame
  mapM_ (draw (V4 255 255 255 250) rnd . fat 1) rs'
  mapM_ (draw (V4 0 255 0 250) rnd) rs'
  SDL.present rnd

  where
    frame = do
      SDL.rendererDrawColor rnd $= V4 255 0 0 200
      drawRect rnd $ Rect (pure 0) size

    blue = V4 0 0 255 100
    red  = V4 255 0 255 100
    draw color rnd rect = do
      SDL.rendererDrawColor rnd $= color
      fillRect rnd rect
      SDL.rendererDrawColor rnd $= V4 255 255 255 200
      drawRect rnd rect
    --
    rs0 = go (mkStdGen seed) numRooms
      where
        go _ 0 = []
        go g i = r' : go g' (i-1)
          where
            (r',g') = makePrimRoom size g
    --
    size = V2 30 30
    numRooms = 50

removeOutRooms :: V2 Double -> [Rect Double] -> [Rect Double]
removeOutRooms (V2 w' h') = filter within
  where
    within (Rect (P (V2 x y)) (V2 w h)) =
      x >= 0 && x + w <= w' && y >= 0 && y + h <= h'

roundPos :: Rect Double -> Rect Double
roundPos (Rect pos size) =
  Rect pos' size
  where
    pos' = (fromIntegral . round) <$> pos

exportRooms :: FilePath -> V2 Double -> [Rect Double] -> IO ()
exportRooms path (V2 w h) rs =
  SVG.export path stg rs'
  where
    stg    = SVG.Settings w h
    fill   = SVG.Fill "#0000ff" 0.4
    stroke = SVG.Stroke "#ffffff" 0.2 0.6
    --
    bg  = SVG.Object (SVG.SRect (Rect (P $ V2 (w/2) (h/2)) (V2 w h)))
                     (SVG.Fill "#000000" 1)
                     (SVG.Stroke "#ffffff" 0 0)
    rs' = bg : map (\r -> SVG.Object (SVG.SRect r) fill stroke) rs
