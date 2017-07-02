module Lib
    ( run
    ) where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

repoURI :: String
repoURI = "https://github.com/python/cpython"

rootPath :: IO FilePath
rootPath = combine <$> getHomeDirectory <*> return ".psla"

run :: IO ()
run = do
  args <-getArgs
  parseArgs args

parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn usage >> exitFailure
parseArgs ("install":args) = install args
parseArgs ("use":args) = use args
parseArgs (x:_) = do
  hPutStrLn stderr $ "psla: no such command " ++ show x
  exitFailure

usage :: String
usage = unlines 
          [ "PSLA is a tool for managing Python environment."
          , ""
          , "Usage:"
          , ""
          , "        install      download and compile the specific version of Python"
          , "        uninstall    uninstall the specific version of Python"
          , "        use          select the specific version of Python as cureent version"
          , ""
          ]

install :: [String] -> IO ()
install [] = hPutStrLn stderr "install: 1 or more arguments required" >> exitFailure
install versions = do
        _ <- createDirectoryIfMissing True <$> (combine <$> rootPath <*> return "repo")
        mapM_ clone versions
        mapM_ build versions

clone :: String -> IO ()
clone version = do
  dest <- foldl1 (liftA2 combine) [rootPath, return "repo", return version]
  doesNotExists <- not <$> doesDirectoryExist dest
  when doesNotExists $
       callProcess "git" ["clone", "--depth", "1", "--branch", version, repoURI, dest]

build :: String -> IO ()
build version = do
  root <- rootPath
  let dest = foldl1 combine [root, "repo", version]
  forM_ [ (dest </> "configure", ["--prefix", foldl1 combine [root, "python", version]])
        , ("make", ["-k", "-j4"])
        , ("make", ["install"])
        ]
        (\(cmd, args) -> do
        (_,_,_,ph) <- createProcess (proc cmd args){ cwd = Just dest }
        code <- waitForProcess ph
        when (code  /= ExitSuccess)
             exitFailure
        )

use :: [String] -> IO ()
use [] = hPutStrLn stderr "use: 1 argument required" >> exitFailure
use [version] = do
  root <- rootPath
  exists <- doesFileExist $ root </> "python" </> version </> "bin" </> "python3"
  unless exists $
         hPutStrLn stderr ( "use: not installed: " ++ show version ) >> exitFailure
  createDirectoryIfMissing True $ root </> "bin"
  let dest = root  </> "bin"  </> "python"
  writeFile dest $ script root version
  perm <- getPermissions dest
  setPermissions dest $ setOwnerExecutable True perm
use _ = hPutStrLn stderr "use: too many arguments" >> exitFailure

script :: String -> String -> String
script root version = unlines [ "#!/bin/sh"
                              , ""
                              , "export PYTHONUSERBASE=" ++ show ( root </> "user" )
                              , show ( root  </> "python"  </> version  </> "bin"  </> "python3" ) ++ " \"$@\""]
