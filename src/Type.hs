module Type
( Rect (..)
, fat
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
