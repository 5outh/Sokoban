<<<<<<< HEAD
{-# LANGUAGE ForeignFunctionInterface #-}
=======
>>>>>>> Idk?
module Paths_Sokoban (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

<<<<<<< HEAD
import Foreign
import Foreign.C
import qualified Control.Exception as Exception
import Data.Version (Version(Version, versionBranch, versionTags))
import System.Environment (getEnv)
import Prelude ((++), FilePath, IO, return)

=======
import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
>>>>>>> Idk?
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
<<<<<<< HEAD
prefix, bindirrel :: FilePath
prefix        = "C:\\Users\\bkovach.CAESAD\\Documents\\MathNotes\\Sokoban\\cabal-dev/"
bindirrel     = "bin"

getBinDir :: IO FilePath
getBinDir = getPrefixDirRel bindirrel

getLibDir :: IO FilePath
getLibDir = getPrefixDirRel "lib\\Sokoban-0.1.0.0/ghc-7.4.2"

getDataDir :: IO FilePath
getDataDir =  catchIO (getEnv "Sokoban_datadir") (\_ -> getPrefixDirRel "Sokoban-0.1.0.0")

getLibexecDir :: IO FilePath
getLibexecDir = getPrefixDirRel "Sokoban-0.1.0.0"
=======
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/macbook/Library/Haskell/ghc-7.4.2/lib/Sokoban-0.1.0.0/bin"
libdir     = "/Users/macbook/Library/Haskell/ghc-7.4.2/lib/Sokoban-0.1.0.0/lib"
datadir    = "/Users/macbook/Library/Haskell/ghc-7.4.2/lib/Sokoban-0.1.0.0/share"
libexecdir = "/Users/macbook/Library/Haskell/ghc-7.4.2/lib/Sokoban-0.1.0.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Sokoban_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Sokoban_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Sokoban_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Sokoban_libexecdir") (\_ -> return libexecdir)
>>>>>>> Idk?

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
<<<<<<< HEAD
  return (dir `joinFileName` name)

getPrefixDirRel :: FilePath -> IO FilePath
getPrefixDirRel dirRel = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return (prefix `joinFileName` dirRel)
          _ | ret < size -> do
              exePath <- peekCWString buf
              let (bindir,_) = splitFileName exePath
              return ((bindir `minusFileName` bindirrel) `joinFileName` dirRel)
            | otherwise  -> try_size (size * 2)

foreign import stdcall unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32

minusFileName :: FilePath -> String -> FilePath
minusFileName dir ""     = dir
minusFileName dir "."    = dir
minusFileName dir suffix =
  minusFileName (fst (splitFileName dir)) (fst (splitFileName suffix))

joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir++fname
  | otherwise                  = dir++pathSeparator:fname

splitFileName :: FilePath -> (String, String)
splitFileName p = (reverse (path2++drive), reverse fname)
  where
    (path,drive) = case p of
       (c:':':p') -> (reverse p',[':',c])
       _          -> (reverse p ,"")
    (fname,path1) = break isPathSeparator path
    path2 = case path1 of
      []                           -> "."
      [_]                          -> path1   -- don't remove the trailing slash if 
                                              -- there is only one character
      (c:path') | isPathSeparator c -> path'
      _                             -> path1

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
=======
  return (dir ++ "/" ++ name)
>>>>>>> Idk?
