module Paths_todo_mvc (
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ollie/.cabal/bin"
libdir     = "/home/ollie/.cabal/lib/x86_64-linux-ghcjs-0.1.0-ghc7_10_0_20150123/todom_I4SdTmAbhBFBAzIm2TzNgh"
datadir    = "/home/ollie/.cabal/share/x86_64-linux-ghcjs-0.1.0-ghc7_10_0_20150123/todo-mvc-0.1"
libexecdir = "/home/ollie/.cabal/libexec"
sysconfdir = "/home/ollie/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "todo_mvc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "todo_mvc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "todo_mvc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "todo_mvc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "todo_mvc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
