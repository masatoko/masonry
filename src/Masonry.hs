module Masonry
( test
) where

import Linear.V2
import System.Random

import Type (Rect (..))
import PrimRoom (mkRoom)

test :: IO ()
test = exportRooms $ map work [0..10]
  where
    work = mkRoom size . mkStdGen
    size = V2 30 20

exportRooms :: [Rect Int] -> IO ()
exportRooms = mapM_ print
