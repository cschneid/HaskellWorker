module Paths_haskellworker (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/cschneid/Projects/opensource/haskellworker/.cabal-sandbox/bin"
libdir     = "/Users/cschneid/Projects/opensource/haskellworker/.cabal-sandbox/lib/x86_64-osx-ghc-7.6.3/haskellworker-0.1.0.0"
datadir    = "/Users/cschneid/Projects/opensource/haskellworker/.cabal-sandbox/share/x86_64-osx-ghc-7.6.3/haskellworker-0.1.0.0"
libexecdir = "/Users/cschneid/Projects/opensource/haskellworker/.cabal-sandbox/libexec"
sysconfdir = "/Users/cschneid/Projects/opensource/haskellworker/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskellworker_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskellworker_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskellworker_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskellworker_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskellworker_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
