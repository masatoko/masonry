module Conf where

import System.IO
import qualified Control.Exception as E
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (isNothing, mapMaybe)
import Safe (readMay, headMay)

data Conf = Conf
  { confWidth        :: Int
  , confHeight       :: Int
  , confExportDir    :: FilePath
  , confExportPrefix :: String
  , confNumRooms     :: Int
  , confNumItWithConst :: Int
  , confNumItWithoutConst :: Int
  , confLengthMu     :: Double
  , confLengthSigma  :: Double
  , confRatioMu      :: Double
  , confRatioSigma   :: Double
  --
  , confVerbose      :: Bool
  , confWaitKey      :: Bool
  } deriving Show

importConf :: FilePath -> IO Conf
importConf path = do
  vmap <- (toAssoc . lines) <$> readUtf8File' path
  case mkConf vmap of
    Left msg   -> error msg
    Right conf -> return conf
  where
    toAssoc = M.fromList . mapMaybe toPair
    toPair line
      | null line      = Nothing
      | isNothing mh   = Nothing
      | mh == Just '#' = Nothing
      | otherwise      = Just (key, val)
      where
        mh = headMay line
        (key:val:_) = splitOn "=" line

    mkConf vmap =
      Conf <$> value "width"
           <*> value "height"
           <*> str   "out_dir"
           <*> str   "out_prefix"
           <*> value "num_rooms"
           <*> value "num_iteration_with_constraint"
           <*> value "num_iteration_without_constraint"
           <*> value "length_mu"
           <*> value "length_sigma"
           <*> value "ratio_mu"
           <*> value "ratio_sigma"
           <*> value "verbose"
           <*> value "waitkey"
      where
        value key =
          case M.lookup key vmap >>= readMay of
            Nothing -> Left $ "Missing key: " ++ key
            Just v  -> Right v

        str key =
          case M.lookup key vmap of
            Nothing -> Left $ "Missing key: " ++ key
            Just v  -> Right v

readUtf8File' :: FilePath -> IO String
readUtf8File' path =
  withFile path ReadMode $ \h -> do
    hSetEncoding h utf8_bom
    cs <- hGetContents h
    mapM_ E.evaluate cs
    return cs
