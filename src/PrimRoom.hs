module PrimRoom
( mkRoom
) where

import System.Random
import Linear.Affine
import Linear.V2

import Type (Rect (..))
import RandNum (normRandom)

mkRoom :: RandomGen g => V2 Int -> g -> Rect Double
mkRoom boundarySize g = Rect pos size
  where
    (pos,g') = mkPoint boundarySize g
    (size,_)  = mkBox fLength fRatio g'

    fLength = normRandom 10 3
    fRatio = randomR (0.2, 0.8)

type RandFunc g = g -> (Double, g)

mkPoint :: RandomGen g => V2 Int -> g -> (Point V2 Double, g)
mkPoint size g = (p, g'')
  where
    (V2 w h) = fromIntegral <$> size
    (x,g')  = normRandom (w/2) (w/4) g
    (y,g'') = normRandom (h/2) (h/4) g'
    -- (x,g')  = randomR (0, w - 1) g
    -- (y,g'') = randomR (0, h - 1) g'
    p = P $ V2 x y

mkBox :: RandomGen g => RandFunc g -> RandFunc g -> g -> (V2 Double, g)
mkBox fLength fRatio g = (V2 w h, g'')
  where
    (len,g')    = fLength g
    (ratio,g'') = fRatio g'
    ratio' = max 0 . min 1 $ ratio
    len' = max 2 len
    w = max 1 $ len' * ratio'
    h = max 1 $ len' * (1 - ratio')
