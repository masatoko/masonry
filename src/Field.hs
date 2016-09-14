module Field
( Field (..)
, Cell (..)
, unionField
, rectToCellField
, dumpFieldBy
, dumpField
) where

import Linear.Affine
import Linear.V2
import Data.List (intercalate)

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
    ps = map ((+ P (V2 0.5 0.5)) . fmap fromIntegral) [P (V2 x y) | y <- [0..(h-1::Int)], x <- [0..(w-1::Int)]]
    --
    toCell p = if p `withinRect` rect
                 then trueCell
                 else Empty

--

dumpFieldBy :: (Cell -> String) -> Int -> Field Cell -> String
dumpFieldBy toStr width (Field cs0) =
  intercalate "," $ map toStr cs0

dumpField :: Int -> Field Cell -> [String]
dumpField width (Field cs0) = go cs0
  where
    go [] = []
    go cs =
      let (as, bs) = splitAt width cs
          line = concatMap toStr as
      in line : go bs

    toStr :: Cell -> String
    toStr Empty = "  "
    toStr Floor = " ."
    toStr Wall  = " #"
