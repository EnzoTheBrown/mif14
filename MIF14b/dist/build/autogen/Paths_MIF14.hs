module Paths_MIF14 (
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

bindir     = "/home/enzo/.cabal/bin"
libdir     = "/home/enzo/.cabal/lib/x86_64-linux-ghc-7.10.3/MIF14-0.1.0.0-Dz1dALWseQNFw7tVcEqCW8"
datadir    = "/home/enzo/.cabal/share/x86_64-linux-ghc-7.10.3/MIF14-0.1.0.0"
libexecdir = "/home/enzo/.cabal/libexec"
sysconfdir = "/home/enzo/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MIF14_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MIF14_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MIF14_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MIF14_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MIF14_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
