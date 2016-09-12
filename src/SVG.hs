module SVG where

import Linear.Affine
import Linear.V2

import Type

data Settings
  = Settings
      { svgWidth :: Int
      , svgHeight :: Int
      } deriving (Show, Read)

data Object =
  Object Shape Fill Stroke
  deriving Show

data Shape =
  SRect (Rect Int)
  deriving Show

type Opacity = Double
type Width = Double
type Color = String

data Fill =
  Fill Color Opacity
  deriving Show

data Stroke =
  Stroke Color Width Opacity
  deriving Show

export :: FilePath -> Settings -> [Object] -> IO ()
export path stg as =
  writeFile path contents
  where
    contents = unlines $ h1:h2:layered shapes ++ ["/svg"]
    --
    h1 = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    h2 = "<svg viewBox=\"0 0 " ++ show w ++ " " ++ show h ++ "\" >"
    shapes = zipWith convert [0..] as
    layered xs = ["<g id=\"layer1\">"] ++ xs ++ ["</g>"]
    --
    w = svgWidth stg
    h = svgHeight stg

convert :: Int -> Object -> String
convert i obj = unlines
  [ "<rect"
  , "x=" ++ dq (x - w `div` 2)
  , "y=" ++ dq (y - h `div` 2)
  , "width=" ++ dq w
  , "height=" ++ dq h
  , "id=" ++ dq i
  , "style=" ++ style
  , "/>"]
  where
    dq a = "\"" ++ show a ++ "\""
    style = "\"opacity:1;"++ convFill fill ++ "fill-rule:nonzero;" ++ convStroke stroke ++ "stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:35.20000076\""
    --
    Object (SRect (Rect pos size)) fill stroke = obj
    P (V2 x y) = pos
    V2 w h     = size

convFill :: Fill -> String
convFill (Fill color opacity) =
  "fill:" ++ color ++ ";fill-opacity:" ++ show opacity ++ ";"

convStroke :: Stroke -> String
convStroke (Stroke color width opacity) =
  "stroke:" ++ color ++ ";stroke-width:" ++ show width ++ ";stroke-opacity:" ++ show opacity ++ ";"
