module PrimRoom
( makePrimRoom
) where

import System.Random
import Linear.Affine
import Linear.V2
import Linear.Vector

import Type (Rect (..))
import RandNum (normRandom)
import Conf

type RandFunc g = g -> (Double, g)

-- Make Room

makePrimRoom :: RandomGen g => Conf -> g -> (Rect Double, g)
makePrimRoom conf g = (Rect pos' size', g'')
  where
    boundarySize = V2 (fromIntegral $ confWidth conf) (fromIntegral $ confHeight conf)
    (pos,g')   = mkPoint boundarySize g
    (size,g'') = mkBox fLength fRatio g'
    pos'  = (fromIntegral . round) <$> (pos - P (size ^/ 2))
    size' = (fromIntegral . round) <$> size

    fLength = normRandom (confLengthMu conf) (confLengthSigma conf)
    fRatio  = normRandom (confRatioMu conf) (confRatioSigma conf)

-- Random Shapes

mkPoint :: RandomGen g => V2 Double -> g -> (Point V2 Double, g)
mkPoint (V2 w h) g = (p, g'')
  where
    (x,g')  = normRandom (w/2) (w/4) g
    (y,g'') = normRandom (h/2) (h/4) g'
    p = P $ V2 x y

mkBox :: RandomGen g => RandFunc g -> RandFunc g -> g -> (V2 Double, g)
mkBox fLength fRatio g = (V2 w h, g'')
  where
    (len,g')    = fLength g
    (ratio,g'') = fRatio g'
    ratio' = max 0.1 . min 0.9 $ ratio
    len' = max 6 len
    h = max 2 $ len' * ratio'
    w = max 2 $ len' * (1 - ratio')
