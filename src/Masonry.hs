module Masonry
( test
) where

import Linear.V2
import System.Random

import PrimRoom (mkRoom)

test :: IO ()
test = mapM_ work [0..10]
  where
    work = print . mkRoom size . mkStdGen
    size = V2 30 20
