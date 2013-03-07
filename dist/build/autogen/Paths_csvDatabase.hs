module Paths_csvDatabase (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/nick/.cabal/bin"
libdir     = "/home/nick/.cabal/lib/csvDatabase-0.0/ghc-7.4.2"
datadir    = "/home/nick/.cabal/share/csvDatabase-0.0"
libexecdir = "/home/nick/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "csvDatabase_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "csvDatabase_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "csvDatabase_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "csvDatabase_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
