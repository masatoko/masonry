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

import Type (Rect (..))
import PrimRoom (makePrimRoom)
import qualified SVG
import Separate.Physics (separateRooms)

import Render

test :: SDL.Renderer -> Int -> IO ()
test rnd seed = do
  let rss = separateRooms size seed rs0
  forM_ (zip [0..] rss) $ \(i,rs) -> do
    when (i `mod` 10 == 0) $ print i
    clearScreen rnd $ V4 0 0 0 255
    frame
    --
    mapM_ (work rnd) rs
    SDL.present rnd
    --
    threadDelay 10000

  let rs' = removeOutRooms size (last rss)
  -- clearScreen rnd $ V4 0 0 0 255
  -- frame
  mapM_ (work rnd . roundPos) rs'
  SDL.present rnd

  where
    frame = do
      SDL.rendererDrawColor rnd $= V4 255 0 0 200
      drawRect rnd $ Rect (pure 0) size
    work rnd rect = do
      SDL.rendererDrawColor rnd $= V4 0 0 255 100
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
