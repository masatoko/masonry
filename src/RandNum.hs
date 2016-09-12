module RandNum
( normRandom
) where

import System.Random

-- let work = normRandom 10 4 . mkStdGen
-- print $ map (fst . work) [0..10]
normRandom :: RandomGen g => Double -> Double -> g -> (Double, g)
normRandom mu sigma g = (z, g'')
  where
    (x, g')  = randomR (0,1) g
    (y, g'') = randomR (0,1) g'
    z = sigma * sqrt (-2 * log x) * cos (2 * pi * y) + mu
