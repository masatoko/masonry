module Separate
( separate
) where

import Linear.Affine
import Linear.V2
import Data.List (foldl')
import Data.Maybe (mapMaybe)

import Type

type Room = Rect Double

separate :: [Room] -> [Room]
separate rs0 = map work irs
  where
    irs = zip [0..] rs0
    --
    work (i,r) = Rect (pos - P delta) size
      where
        (Rect pos size) = r
        --
        rs' = map snd $ filter ((/= i) . fst) irs
        vs = mapMaybe (penetration r) rs'
        --
        n = fromIntegral $ 1 `max` length vs
        delta = (/ (10 * n)) <$> sum vs

penetration :: Room -> Room -> Maybe (V2 Double)
penetration ra rb =
  V2 <$> (absMin <$> penx0 <*> penx1)
     <*> (absMin <$> peny0 <*> peny1)
  where
    (V2 a0x a0y, V2 a1x a1y) = box ra
    (V2 b0x b0y, V2 b1x b1y) = box rb
    --
    penx0 = fmap negate . work $ b1x - a0x
    penx1 = work $ a1x - b0x
    peny0 = fmap negate . work $ b1y - a0y
    peny1 = work $ a1y - b0y
    --
    work x = if x >= 0 then Just x else Nothing
    absMin x y = if abs x < abs y then x else y

box (Rect (P (V2 x y)) (V2 w h)) =
  (V2 (x - w') (y - h'), V2 (x + w') (y + h'))
  where
    w' = w / 2
    h' = h / 2