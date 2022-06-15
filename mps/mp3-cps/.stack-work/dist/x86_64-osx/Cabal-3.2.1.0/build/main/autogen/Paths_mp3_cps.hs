{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp3_cps (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/fuguanshujie/Desktop/ZJUI/4_2022SPRING/CS421/git_work/sp22_cs421_gf9/mps/mp3-cps/.stack-work/install/x86_64-osx/71b89e87d74e40a54b928e77d8aabe36cb88c71051608aa506907f12c1225ec8/8.10.7/bin"
libdir     = "/Users/fuguanshujie/Desktop/ZJUI/4_2022SPRING/CS421/git_work/sp22_cs421_gf9/mps/mp3-cps/.stack-work/install/x86_64-osx/71b89e87d74e40a54b928e77d8aabe36cb88c71051608aa506907f12c1225ec8/8.10.7/lib/x86_64-osx-ghc-8.10.7/mp3-cps-0.1.0.0-IMBc5LwlISA3eMUjedOPcS-main"
dynlibdir  = "/Users/fuguanshujie/Desktop/ZJUI/4_2022SPRING/CS421/git_work/sp22_cs421_gf9/mps/mp3-cps/.stack-work/install/x86_64-osx/71b89e87d74e40a54b928e77d8aabe36cb88c71051608aa506907f12c1225ec8/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/fuguanshujie/Desktop/ZJUI/4_2022SPRING/CS421/git_work/sp22_cs421_gf9/mps/mp3-cps/.stack-work/install/x86_64-osx/71b89e87d74e40a54b928e77d8aabe36cb88c71051608aa506907f12c1225ec8/8.10.7/share/x86_64-osx-ghc-8.10.7/mp3-cps-0.1.0.0"
libexecdir = "/Users/fuguanshujie/Desktop/ZJUI/4_2022SPRING/CS421/git_work/sp22_cs421_gf9/mps/mp3-cps/.stack-work/install/x86_64-osx/71b89e87d74e40a54b928e77d8aabe36cb88c71051608aa506907f12c1225ec8/8.10.7/libexec/x86_64-osx-ghc-8.10.7/mp3-cps-0.1.0.0"
sysconfdir = "/Users/fuguanshujie/Desktop/ZJUI/4_2022SPRING/CS421/git_work/sp22_cs421_gf9/mps/mp3-cps/.stack-work/install/x86_64-osx/71b89e87d74e40a54b928e77d8aabe36cb88c71051608aa506907f12c1225ec8/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp3_cps_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp3_cps_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp3_cps_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp3_cps_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp3_cps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp3_cps_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
