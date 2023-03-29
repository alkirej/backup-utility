{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_backup_util (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jeff/.cabal/bin"
libdir     = "/home/jeff/.cabal/lib/x86_64-linux-ghc-8.8.4/backup-util-1.0.0.0-inplace-backup-util"
dynlibdir  = "/home/jeff/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/jeff/.cabal/share/x86_64-linux-ghc-8.8.4/backup-util-1.0.0.0"
libexecdir = "/home/jeff/.cabal/libexec/x86_64-linux-ghc-8.8.4/backup-util-1.0.0.0"
sysconfdir = "/home/jeff/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "backup_util_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "backup_util_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "backup_util_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "backup_util_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "backup_util_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "backup_util_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
