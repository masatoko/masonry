module Type
( Rect (..)
, fat
, withinRect
) where

import Linear.Affine
import Linear.V2

data Rect a =
  Rect (Point V2 a) (V2 a) -- Rect LeftTop Size
  deriving Show

fat :: Num a => a -> Rect a -> Rect a
fat a (Rect pos size) = Rect pos' size'
  where
    pos'  = pos - pure a
    size' = size + pure (2 * a)

withinRect :: (Num a, Ord a) => Point V2 a -> Rect a -> Bool
withinRect (P (V2 x y)) (Rect pos size) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1
  where
    P (V2 x0 y0) = pos
    V2 w h = size
    x1 = x0 + w
    y1 = y0 + h
