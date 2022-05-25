{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_endgame2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Ole\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Ole\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.2.1\\endgame2-0.1.0.0-inplace-endgame2"
dynlibdir  = "C:\\Users\\Ole\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.2.1"
datadir    = "C:\\Users\\Ole\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.2.1\\endgame2-0.1.0.0"
libexecdir = "C:\\Users\\Ole\\AppData\\Roaming\\cabal\\endgame2-0.1.0.0-inplace-endgame2\\x86_64-windows-ghc-9.2.1\\endgame2-0.1.0.0"
sysconfdir = "C:\\Users\\Ole\\AppData\\Roaming\\cabal\\etc"

getBinDir     = catchIO (getEnv "endgame2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "endgame2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "endgame2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "endgame2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "endgame2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "endgame2_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
