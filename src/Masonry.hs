module Masonry
( test
) where

import Control.Monad (forM_, when)
import Linear.Affine
import Linear.V2
import System.Random
import Data.List (scanl')

import Type (Rect (..))
import PrimRoom (makePrimRoom)
import qualified SVG
import Separate (separate)

test :: Int -> IO ()
test seed = do
  exportRooms "c:/_temp/rooms.svg" size rs0
  forM_ (zip [0..] rss) $ \(i,rs) ->
    when (i `mod` 100 == 0) $ do
      let path = "c:/_temp/rooms" ++ show i ++ ".svg"
      exportRooms path size rs
  where
    rs0 = go (mkStdGen seed) 30
    rss = scanl' (\a _ -> separate a) rs0 [0..1000]
    --
    size = V2 30 30
    --
    go _ 0 = []
    go g i = r' : go g' (i-1)
      where
        (r',g') = makePrimRoom size g

exportRooms :: FilePath -> V2 Int -> [Rect Double] -> IO ()
exportRooms path (V2 w h) rs =
  SVG.export path stg rs'
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
