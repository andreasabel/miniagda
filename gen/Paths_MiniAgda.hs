{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_MiniAgda (
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
version = Version [0,2022,4,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/abel/.cabal/bin"
libdir     = "/Users/abel/.cabal/lib/x86_64-osx-ghc-9.2.2/MiniAgda-0.2022.4.6-inplace-miniagda"
dynlibdir  = "/Users/abel/.cabal/lib/x86_64-osx-ghc-9.2.2"
datadir    = "/Users/abel/.cabal/share/x86_64-osx-ghc-9.2.2/MiniAgda-0.2022.4.6"
libexecdir = "/Users/abel/.cabal/libexec/x86_64-osx-ghc-9.2.2/MiniAgda-0.2022.4.6"
sysconfdir = "/Users/abel/.cabal/etc"

getBinDir     = catchIO (getEnv "MiniAgda_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "MiniAgda_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "MiniAgda_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "MiniAgda_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MiniAgda_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MiniAgda_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
