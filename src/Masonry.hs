module Masonry
( test
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Linear.Affine
import Linear.V2
import Linear.V4
import System.Random
import Data.List (scanl')

import SDL (($=))
import qualified SDL

import Type (Rect (..))
import PrimRoom (makePrimRoom)
import qualified SVG
import Separate (separate)

import Render

test :: SDL.Renderer -> Int -> IO ()
test rnd seed =
  forM_ (zip [0..] rss) $ \(i,rs) -> do
    when (i `mod` 10 == 0) $ print i
    clearScreen rnd $ V4 0 0 0 255
    mapM_ (work rnd) rs
    SDL.present rnd
    --
    threadDelay 50000
  where
    work rnd rect = do
      SDL.rendererDrawColor rnd $= V4 0 0 255 100
      fillRect rnd rect
      SDL.rendererDrawColor rnd $= V4 255 255 255 200
      drawRect rnd rect
    --
    rs0 = go (mkStdGen seed) numRooms
    rss = scanl' (\a _ -> separate size a) rs0 [0..numIteration]
    --
    size = V2 30 30
    numRooms = 50
    numIteration = 200
    --
    go _ 0 = []
    go g i = r' : go g' (i-1)
      where
        (r',g') = makePrimRoom size g

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
