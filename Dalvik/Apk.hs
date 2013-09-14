module Dalvik.Apk where

import Codec.Archive.Zip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Binary
import System.IO

import Dalvik.Parser
import Dalvik.Types

loadDexFromApkIO :: FilePath -> IO (Either String DexFile)
loadDexFromApkIO f = do
  chunks <- withArchive f (sourceEntry "classes.dex" sinkLbs)
  -- TODO: this is silly. Should we tweak the parser to work with
  -- lazy ByteStrings?
  return . loadDex $ chunks

loadDexFromAnyIO :: FilePath -> IO (Either String DexFile)
loadDexFromAnyIO f = do
  h <- openFile f ReadMode
  c <- hGetChar h
  hClose h
  case c of
    'P' -> loadDexFromApkIO f
    'd' -> loadDexIO f
    _ -> return (Left "Invalid file format")
