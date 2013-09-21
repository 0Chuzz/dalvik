module Dalvik.Dex.Maps where

import Data.Word
import Data.Map
import Data.ByteString.Lazy hiding (zip)
import Data.Binary.Get
import Control.Monad

data MapItem
  = MapItem {
      itemType :: Word16
    , itemSize :: Word32
    , itemOff  :: Word32
    } deriving (Show)

{- Section parsing -}

parseMap :: ByteString -> Word32 -> Get (Map Word32 MapItem)
parseMap _ _ = do
  size <- getWord32le
  items <- replicateM (fromIntegral size) parseMapItem
  return . fromList . zip [0..] $ items

parseMapItem :: Get MapItem
parseMapItem = do
  iType <- getWord16le
  _unused <- getWord16le
  iSize <- getWord32le
  iOff <- getWord32le
  return $ MapItem iType iSize iOff

