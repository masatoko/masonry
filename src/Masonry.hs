module Masonry
( generate
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Linear.Affine
import Linear.V2
import Linear.V4
import Linear.Vector
import System.Random
import Data.List (scanl')
import Foreign.C.String (withCString)

import SDL (($=))
import qualified SDL
import SDL.Raw (saveBMP)

import Type (Rect (..), fat)
import PrimRoom (makePrimRoom)
import qualified SVG
import Separate.Physics (separateRooms)
import Conf
import Field

import Render

generate :: FilePath -> Conf -> SDL.Renderer -> Int -> IO ()
generate pathExport conf rnd seed = do
  clearScreen rnd black
  when (confVerbose conf) . forM_ rs0 $ \r -> do
      draw blue rnd r
      SDL.present rnd
      threadDelay 30000
  let drawRss rss =
        forM_ (zip [0..] rss) $ \(i,rs) ->
          when (go i) $ do
            clearScreen rnd black
            frame
            --
            mapM_ (draw blue rnd) rs
            SDL.present rnd
            --
            threadDelay 30000

  let rss0 = separateRooms conf 200 0.1 rs0
      rss1 = separateRooms conf 500 0     $ last rss0
  drawRss rss0
  drawRss rss1

  -- Render
  let rs' = map roundPos $ removeOutRooms size (last rss1)
  SDL.rendererDrawColor rnd $= V4 0 0 0 200
  drawRect rnd $ Rect (pure 0) size
  frame
  mapM_ (draw (V4 255 255 255 250) rnd . fat 1) rs'
  mapM_ (draw (V4 0 255 0 250) rnd) rs'
  SDL.present rnd
  -- Render

  -- Rasterise
  let wallField = foldl1 unionField . map (rectToCellField Wall size . fat 1) $ rs'
      floorField = foldl1 unionField . map (rectToCellField Floor size) $ rs'
      field = wallField `unionField` floorField
  let pretty = dumpField (confWidth conf) field
  mapM_ putStrLn pretty
  let pathPretty = pathExport ++ "_" ++ show seed ++ "_pretty.txt"
      path = pathExport ++ "_" ++ show seed ++ "_data.txt"
  writeFile pathPretty . unlines $ pretty
  writeFile path . unlines $ dumpFieldBy cellToIndex (confWidth conf) field
  -- Rasterise

  where
    size = V2 (fromIntegral $ confWidth conf) (fromIntegral $ confHeight conf)
    numRooms = confNumRooms conf
    --
    go i
      | not (confVerbose conf) = False
      | i < 100                = i `mod` 3 == 0
      | otherwise              = i `mod` 10 == 0

    frame = do
      SDL.rendererDrawColor rnd $= V4 255 0 0 200
      drawRect rnd $ Rect (pure 0) size

    black = V4 0 0 0 255
    blue  = V4 0 0 255 100
    red   = V4 255 0 255 100
    draw color rnd rect = do
      SDL.rendererDrawColor rnd $= color
      fillRect rnd rect
      SDL.rendererDrawColor rnd $= V4 255 255 255 100
      drawRect rnd rect
    --
    rs0 = go (mkStdGen seed) numRooms
      where
        go _ 0 = []
        go g i = r' : go g' (i-1)
          where
            (r',g') = makePrimRoom conf g

removeOutRooms :: V2 Double -> [Rect Double] -> [Rect Double]
removeOutRooms (V2 w' h') = filter within
  where
    within rect =
      let (Rect (P (V2 x y)) (V2 w h)) = fat 1 rect
      in x >= 0 && x + w <= w' && y >= 0 && y + h <= h'

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

cellToIndex :: Cell -> String
cellToIndex Empty = "0"
cellToIndex Floor = "10"
cellToIndex Wall  = "100"

--

exportSurface :: SDL.Surface -> String -> IO ()
exportSurface surface path =
  withCString path $ \cstr -> do
    saveBMP ps cstr
    return ()
  where
    SDL.Surface ps _ = surface
