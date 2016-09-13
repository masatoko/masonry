module Render where

import qualified SDL

import Type

drawRect :: SDL.Renderer -> Rect Double -> IO ()
drawRect r (Rect pos size) =
  SDL.drawRect r $ Just rect
  where
    pos' = round <$> pos
    size' = round <$> size
    rect = SDL.Rectangle pos' size'
