module Paths_flipper (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/George/.cabal/bin"
libdir     = "/Users/George/.cabal/lib/x86_64-osx-ghc-7.10.3/flipper-0.1.0.0-3152uWnkWA6GDLOe1EtEeI"
datadir    = "/Users/George/.cabal/share/x86_64-osx-ghc-7.10.3/flipper-0.1.0.0"
libexecdir = "/Users/George/.cabal/libexec"
sysconfdir = "/Users/George/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "flipper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "flipper_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "flipper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "flipper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "flipper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
