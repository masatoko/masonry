module Conf where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (isNothing, mapMaybe)
import Safe (readMay, headMay)

data Conf = Conf
  { confWidth       :: Int
  , confHeight      :: Int
  , confNumRooms    :: Int
  --
  , confVerbose     :: Bool
  --
  , confLengthMu    :: Double
  , confLengthSigma :: Double
  , confRatioMu     :: Double
  , confRatioSigma  :: Double
  } deriving Show

importConf :: FilePath -> IO Conf
importConf path = do
  vmap <- (toAssoc . lines) <$> readFile path
  case mkConf vmap of
    Nothing   -> error "Cannot parse config file"
    Just conf -> return conf
  where
    toAssoc = M.fromList . mapMaybe toPair
    toPair line
      | isNothing mh   = Nothing
      | mh == Just '#' = Nothing
      | otherwise      = Just (key, val)
      where
        mh = headMay line
        (key:val:_) = splitOn "=" line

    mkConf vmap =
      Conf <$> value "width"
           <*> value "height"
           <*> value "num_rooms"
           <*> value "verbose"
           <*> value "length_mu"
           <*> value "length_sigma"
           <*> value "ratio_mu"
           <*> value "ratio_sigma"
      where
        value key = M.lookup key vmap >>= readMay
