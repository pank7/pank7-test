module Paths_Craft3e (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,10], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/pank7/.cabal/bin"
libdir     = "/Users/pank7/.cabal/lib/Craft3e-0.1.0.10/ghc-7.6.3"
datadir    = "/Users/pank7/.cabal/share/Craft3e-0.1.0.10"
libexecdir = "/Users/pank7/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Craft3e_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Craft3e_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Craft3e_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Craft3e_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
