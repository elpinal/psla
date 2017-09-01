module Lib
    ( run
    ) where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Process

repoURI :: String
repoURI = "https://github.com/python/cpython"

rootPath :: IO FilePath
rootPath = combine <$> getHomeDirectory <*> return ".psla"

run :: IO ()
run = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn usage >> exitFailure
parseArgs (name:args) = cmd name args
  where
    cmd "help" = help
    cmd "install" = install
    cmd "use" = use
    cmd "list" = list
    cmd "uninstall" = uninstall
    cmd x = \_ -> do
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
          , "        help         show help"
          , ""
          ]

help :: [String] -> IO ()
help [] = putStrLn usage
help ["install"] = putStrLn "usage: psla install versions..."
help ["uninstall"] = putStrLn "usage: psla uninstall versions..."
help ["use"] = putStrLn "usage: psla use version"
help ["list"] = putStrLn "usage: psla list"
help [topic] = do
  hPutStrLn stderr $ "unknown help topic " ++ show topic ++ ". Run 'psla help'."
  exitFailure
help _ = do
  hPutStrLn stderr $
            unlines [ "usage: psla help command"
                    , ""
                    , "Too many arguments given."
                    ]
  exitFailure

data Flag =
    Config String
  | Framework
    deriving Show

installFlags :: State [String] (Maybe Flag)
installFlags = do
  x : xs <- get
  case x of
    "-config" -> do
      put $ tail xs
      return $ return $ Config $ head xs
    "-framework" -> do
      put xs
      return $ return Framework
installFlags = return Nothing

parseFlag :: State [String] (Maybe Flag) -> State [String] [Flag]
parseFlag s = do
  flag <- s
  case flag of
    Just x -> do
      xs <- parseFlag s
      return $ x:xs
    Nothing -> parseFlag s

install :: [String] -> IO ()
install [] = hPutStrLn stderr "install: 1 or more arguments required" >> exitFailure
install args = do
  (flags, versions) <- runState $ parseFlag installFlags args
  _ <- createDirectoryIfMissing True <$> (combine <$> rootPath <*> return "repo")
  mapM_ clone versions
  mapM_ ( build flags ) versions

clone :: String -> IO ()
clone version = do
  dest <- foldl1 (liftA2 combine) [rootPath, return "repo", return version]
  doesNotExists <- not <$> doesDirectoryExist dest
  when doesNotExists $
       callProcess "git" ["clone", "--depth", "1", "--branch", version, repoURI, dest]

build :: [Flag] -> String -> IO ()
build flags version = do
  root <- rootPath
  let dest = foldl1 combine [root, "repo", version]
  forM_ [ (dest </> "configure", configOpt ++ frameworkOpt root ++ ["--prefix", foldl1 combine [root, "python", version]])
        , ("make", ["-k", "-j4"])
        , ("make", ["install"])
        ]
        (\(cmd, args) -> do
          (_,_,_,ph) <- createProcess (proc cmd args){ cwd = Just dest }
          code <- waitForProcess ph
          when (code /= ExitSuccess)
               exitFailure
        )
  where
       isConfig (Config _) = True
       isConfig _ = False
       fromConfig (Config x) = x
       configOpt = map fromConfig $ filter isConfig flags
       frameworkOpt root =
         if Framework `elem` flags then
           [("--enable-framework=" ++ (root </> "frameworks" </> version))]
         else
           []

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
script root version =
  unlines [ "#!/bin/sh"
          , ""
          , "export PYTHONUSERBASE=" ++ show ( root </> "user" </> version )
          , show ( root  </> "python"  </> version  </> "bin"  </> "python3" ) ++ " \"$@\""
          ]

list :: [String] -> IO ()
list [] = do
  root <- rootPath
  dirs <- listDirectory $ root </> "python"
  mapM_ putStrLn dirs
list _ = hPutStrLn stderr "usage: list" >> exitFailure

uninstall :: [String] -> IO ()
uninstall [] = hPutStrLn stderr "usage: psla uninstall versions..." >> exitFailure
uninstall versions = do
  root <- rootPath
  forM_ versions (\v -> forM_ ["repo", "python", "frameworks", "user"]
                              (\dir -> removeDirectoryRecursive ( root  </> dir  </> v )
                                       `catch`
                                       (\e -> unless ( isDoesNotExistError  e ) $ throw e)))
