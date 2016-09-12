module Masonry
( test
) where

import Linear.V2
import System.Random

import Type (Rect (..))
import PrimRoom (mkRoom)
import qualified SVG as SVG

test :: IO ()
test = exportRooms $ map work [0..30]
  where
    work = mkRoom size . mkStdGen
    size = V2 30 20

exportRooms :: [Rect Double] -> IO ()
exportRooms rs =
  SVG.export "rooms.svg" stg rs'
  where
    stg    = SVG.Settings 30 20
    fill   = SVG.Fill "#0000ff" 0.2
    stroke = SVG.Stroke "#000000" 0.1 0.5
    rs'    = map (\r -> SVG.Object (SVG.SRect r) fill stroke) rs
