{-# LANGUAGE MultiWayIf #-}

module Separate.Physics
( separateRooms
, separate
) where

import System.Random
import Linear.Affine
import Linear.V2
import Data.List (foldl', sortBy, scanl')
import Data.Maybe (mapMaybe, fromMaybe)
import Safe (lastMay)

import Type
import Conf

type Room = Rect Double

separateRooms :: Conf -> Int -> Double -> [Room] -> [[Room]]
separateRooms conf numIteration hold rs0 =
  scanl' (\a _ -> separate conf hold a) rs0 [0..numIteration]

separate :: Conf -> Double -> [Room] -> [Room]
separate conf hold rs0 = map work irs
  where
    w0 = fromIntegral $ confWidth conf
    h0 = fromIntegral $ confHeight conf
    irs = zip [0..] rs0
    --
    work (i,r) = Rect (pos + P delta) size
      where
        (Rect pos size) = r
        --
        rs' = map snd $ filter ((/= i) . fst) irs
        vs = mapMaybe (exclusion r) rs'
        --
        n = fromIntegral $ 1 `max` length vs
        delta = (/ n) <$> sum vs
        ordAbs x y = work x `compare` work y
          where
            work (V2 a b) = abs a + abs b
        -- delta = fromMaybe (pure 0) . lastMay $ sortBy ordAbs vs

    exclusion :: Room -> Room -> Maybe (V2 Double)
    exclusion ra rb = ((outer +) . work) <$> mv
      where
        ra' = fat 0.5 ra
        rb' = fat 0.5 rb
        mv = penetration ra' rb'
        work = fmap (negate . (/3)) . minpen
        minpen (V2 x y) =
          if abs x < abs y
            then V2 x 0
            else V2 0 y

        (Rect (P (V2 x y)) (V2 w h)) = ra
        vx = if | x < 0      -> V2 hold hold
                | x + w > w0 -> V2 (-hold) (-hold)
                | otherwise  -> pure 0
        vy = if | y < 0      -> V2 (-hold) hold
                | y + h > h0 -> V2 hold (-hold)
                | otherwise  -> pure 0
        outer = vx + vy

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

    box :: Num a => Rect a -> (V2 a, V2 a)
    box (Rect (P (V2 x y)) (V2 w h)) = (V2 x y, V2 (x + w) (y + h))
