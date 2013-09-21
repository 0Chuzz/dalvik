module Dalvik.Dex.String where

import Prelude hiding (drop)
import Data.ByteString.Lazy hiding (zip)
import qualified Data.ByteString as BS
import Data.Map
import Data.Word
import Data.Binary.Get
import Control.Monad
import Control.Applicative

import Dalvik.LEB128

type StringId = Word32
type StringIdJumbo = Word32

subGet :: Integral c => ByteString -> c -> Get a -> Get a
subGet bs off p = return $ runGet p $ drop (fromIntegral off) bs

subGet' :: Integral c => ByteString -> c -> a -> Get a -> Get a
subGet' _ 0 def _ = return def
subGet' bs off _ p = subGet bs off p

parseStrings :: ByteString -> Word32 -> Get (Map Word32 BS.ByteString)
parseStrings bs size = do
  offs <- replicateM (fromIntegral size) getWord32le
  strs <- mapM (\off -> subGet bs off parseStringDataItem) offs
  return . fromList . zip [0..] $ strs

parseStringDataItem :: Get BS.ByteString
parseStringDataItem = getULEB128 >>=  const (BS.pack <$> go)
  where go = do c <- getWord8
                if c == 0 then return [] else (c:) <$> go

