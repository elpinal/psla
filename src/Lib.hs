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

getRootPath :: IO FilePath
getRootPath = do
  home <- getHomeDirectory
  return $ home </> ".psla"

run :: IO ()
run = do
  args <- getArgs
  parseArgs args

fail :: String -> IO ()
fail msg = hPutStrLn stderr msg >> exitFailure

parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn usage >> exitFailure
parseArgs (name:args) = cmd name args
  where
    cmd "help" = help
    cmd "install" = install
    cmd "use" = use
    cmd "list" = list
    cmd "uninstall" = uninstall
    cmd x = \_ -> fail $ "psla: no such command " ++ show x

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
help [topic] = helpOf topic
  where
    helpOf "install" = putStrLn "usage: psla install versions..."
    helpOf "uninstall" = putStrLn "usage: psla uninstall versions..."
    helpOf "use" = putStrLn "usage: psla use version"
    helpOf "list" = putStrLn "usage: psla list"
    helpOf topic = fail $ "unknown help topic " ++ show topic ++ ". Run 'psla help'."
help _ =
  fail $ unlines [ "usage: psla help command"
                 , ""
                 , "Too many arguments given."
                 ]

data Flag =
    Config String
  | Framework
    deriving Show

installFlags :: State [String] (Maybe Flag)
installFlags = do
  x : xs <- get
  return $ case x of
    "-config" -> do
      put $ tail xs
      return $ Config $ head xs
    "-framework" -> do
      put xs
      return Framework
    _ -> do
      put $ x:xs
      Nothing

parseFlag :: State [String] (Maybe Flag) -> State [String] [Flag]
parseFlag s = do
  flag <- s
  maybe (return []) parseRest flag
    where
      parseRest x = do
        xs <- parseFlag s
        return $ x:xs

install :: [String] -> IO ()
install [] = fail "install: 1 or more arguments required"
install args = do
  (flags, versions) <- runState $ parseFlag installFlags args
  root <- getRootPath
  createDirectoryIfMissing True $ root </> "repo"
  mapM_ clone versions
  mapM_ (build flags) versions

getDest :: String -> IO FilePath
  root <- getRootPath
  return $ foldl1 combine [root, "repo", version]

clone :: String -> IO ()
clone version = do
  dest <- getDest version
  doesNotExists <- not <$> doesDirectoryExist dest
  when doesNotExists $
       callProcess "git" ["clone", "--depth", "1", "--branch", version, repoURI, dest]

build :: [Flag] -> String -> IO ()
build flags version = do
  root <- getRootPath
  dest <- getDest version
  mapM_ (exec dest)
        [ (dest </> "configure", configOpt ++ frameworkOpt root ++ ["--prefix", foldl1 combine [root, "python", version]])
        , ("make", ["-k", "-j4"])
        , ("make", ["install"])
        ]
    where
      getConfig (Config x) = Just x
      getConfig _ = Nothing
      configOpt = mapMaybe getConfig flags
      frameworkOpt root =
        if Framework `elem` flags then
          [("--enable-framework=" ++ (root </> "frameworks" </> version))]
        else
          []
      exec dest (cmd, args) = do
        (_, _, _, ph) <- createProcess (proc cmd args){ cwd = Just dest }
        code <- waitForProcess ph
        when (code /= ExitSuccess)
             exitFailure

use :: [String] -> IO ()
use [] = fail "use: 1 argument required"
use [version] = do
  root <- getRootPath
  exists <- doesFileExist $ foldl1 combine [root, "python", version, "bin", "python3"]
  unless exists $
         fail $ "use: not installed: " ++ show version 
  createDirectoryIfMissing True $ root </> "bin"
  let dest = root  </> "bin"  </> "python"
  writeFile dest $ script root version
  perm <- getPermissions dest
  setPermissions dest $ setOwnerExecutable True perm
use _ = fail "use: too many arguments"

script :: String -> String -> String
script root version =
  unlines [ "#!/bin/sh"
          , ""
          , "export PYTHONUSERBASE=" ++ show (root </> "user" </> version)
          , show (foldl1 combine [root, "python", version, "bin", "python3"]) ++ " \"$@\""
          ]

list :: [String] -> IO ()
list [] = do
  root <- getRootPath
  dirs <- listDirectory $ root </> "python"
  mapM_ putStrLn dirs
list _ = fail "usage: list"

uninstall :: [String] -> IO ()
uninstall [] = fail "usage: psla uninstall versions..."
uninstall versions = do
  mapM_ remove versions
    where
      remove v = mapM_ (removeDirs v) ["repo", "python", "frameworks", "user"]
      removeDirs v dir = do
        root <- getRootPath
        removeDirectoryRecursive (root </> dir </> v)
          `catch` \e -> unless (isDoesNotExistError e)
                               (throw e)
