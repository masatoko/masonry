module Type
( Rect (..)
) where

import Linear.Affine
import Linear.V2

data Rect a =
  Rect (Point V2 a) (V2 a) -- Rect Center Size
  deriving Show
