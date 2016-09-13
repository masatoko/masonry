module Field
( Field (..)
, Cell (..)
, unionField
, rectToCellField
, dumpField
) where

import Linear.Affine
import Linear.V2

import Type

data Field a
  = Field [a]
  deriving Show

data Cell
  = Empty
  | Floor
  | Wall
  deriving Show

--

unionField :: Field Cell -> Field Cell -> Field Cell
unionField (Field cs0) (Field cs1) =
  Field $ zipWith combine cs0 cs1
  where
    combine :: Cell -> Cell -> Cell
    combine Empty a     = a
    combine a     Empty = a
    combine Floor Wall  = Floor
    combine Wall  Floor = Floor
    combine a     _     = a

rectToCellField :: Cell -> V2 Double -> Rect Double -> Field Cell
rectToCellField trueCell size rect =
  Field $ map toCell ps
  where
    V2 w h = round <$> size
    ps = map ((+ P (V2 0.5 0.5)) . fmap fromIntegral) $ [P (V2 x y) | y <- [0..(h::Int)], x <- [0..(w::Int)]]
    --
    toCell p = if p `withinRect` rect
                 then trueCell
                 else Empty

--

dumpField :: Int -> Field Cell -> [String]
dumpField width (Field cs0) = go cs0
  where
    go [] = []
    go cs =
      let (as, bs) = splitAt (width + 1) cs
          line = concatMap toChar as
      in line : go bs

    toChar :: Cell -> String
    toChar Empty = " _"
    toChar Floor = " ."
    toChar Wall  = " #"
