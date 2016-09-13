module PrimRoom
( makePrimRoom
) where

import System.Random
import Linear.Affine
import Linear.V2

import Type (Rect (..))
import RandNum (normRandom)

type RandFunc g = g -> (Double, g)

-- Make Room

makePrimRoom :: RandomGen g => V2 Double -> g -> (Rect Double, g)
makePrimRoom boundarySize g = (Rect pos size, g'')
  where
    (pos,g')   = mkPoint boundarySize g
    (size,g'') = mkBox fLength fRatio g'

    fLength = normRandom 8 4
    fRatio  = normRandom 0.5 0.2

-- Random Shapes

mkPoint :: RandomGen g => V2 Double -> g -> (Point V2 Double, g)
mkPoint (V2 w h) g = (p, g'')
  where
    (x,g')  = normRandom (w/2) (w/8) g
    (y,g'') = normRandom (h/2) (h/8) g'
    p = P $ V2 x y

mkBox :: RandomGen g => RandFunc g -> RandFunc g -> g -> (V2 Double, g)
mkBox fLength fRatio g = (V2 w h, g'')
  where
    (len,g')    = fLength g
    (ratio,g'') = fRatio g'
    ratio' = max 0.1 . min 0.9 $ ratio
    len' = max 2 len
    w = max 1 $ len' * ratio'
    h = max 1 $ len' * (1 - ratio')
