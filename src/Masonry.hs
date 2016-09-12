module Masonry
( test
) where

import Linear.Affine
import Linear.V2
import System.Random

import Type (Rect (..))
import PrimRoom (mkRoom)
import qualified SVG

test :: Int -> IO ()
test seed =
  exportRooms size $ go g0 30
  where
    g0 = mkStdGen seed
    size = V2 30 30
    --
    go _ 0 = []
    go g i = r' : go g' (i-1)
      where
        (r',g') = mkRoom size g

exportRooms :: V2 Int -> [Rect Double] -> IO ()
exportRooms (V2 w h) rs =
  SVG.export "c:/_temp/rooms.svg" stg rs'
  where
    stg    = SVG.Settings w h
    fill   = SVG.Fill "#0000ff" 0.4
    stroke = SVG.Stroke "#ffffff" 0.2 0.6
    --
    w' = fromIntegral w
    h' = fromIntegral h
    bg  = SVG.Object (SVG.SRect (Rect (P $ V2 (w'/2) (h'/2)) (V2 w' h')))
                     (SVG.Fill "#000000" 1)
                     (SVG.Stroke "#ffffff" 0 0)
    rs' = bg : map (\r -> SVG.Object (SVG.SRect r) fill stroke) rs
