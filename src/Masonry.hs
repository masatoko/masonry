module Masonry
( test
) where

import Linear.V2
import System.Random

import Type (Rect (..))
import PrimRoom (mkRoom)
import qualified SVG as SVG

test :: IO ()
test = exportRooms $ map work [0..10]
  where
    work = mkRoom size . mkStdGen
    size = V2 30 20

exportRooms :: [Rect Int] -> IO ()
exportRooms rs =
  SVG.export "rooms.svg" stg rs'
  where
    stg    = SVG.Settings 30 20
    fill   = SVG.Fill "#000000" 0.5
    stroke = SVG.Stroke "#000000" 0.1 0.5
    rs'    = map (\r -> SVG.Object (SVG.SRect r) fill stroke) rs
