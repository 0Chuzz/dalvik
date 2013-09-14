module Dalvik.Apk where

import Codec.Archive.Zip
import Data.Conduit.Binary
import System.IO

import Dalvik.Parser
import Dalvik.Types

loadDexFromApkIO :: FilePath -> IO (Either String DexFile)
loadDexFromApkIO f = do
  chunks <- withArchive f (sourceEntry "classes.dex" sinkLbs)
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
