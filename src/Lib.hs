module Lib where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Except
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
  root <- getRootPath
  args <- getArgs
  parseArgs root args

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr msg >> exitFailure

parseArgs :: FilePath -> [String] -> IO ()
parseArgs _ [] = putStrLn usage >> exitFailure
parseArgs root (name:args) = cmd name root args
  where
    cmd "help" = help
    cmd "install" = install
    cmd "use" = use
    cmd "list" = list
    cmd "uninstall" = uninstall
    cmd x = nocmd x

nocmd :: String -> a -> b -> IO ()
nocmd x _ _ = failWith $ "psla: no such command " ++ show x

usage :: String
usage =
  unlines [ "PSLA is a tool for managing Python environment."
          , ""
          , "Usage:"
          , ""
          , "        psla [flags] command [arguments]"
          , ""
          , "Commands:"
          , ""
          , "        help       show help"
          , "        install    download and compile the specific version of Python"
          , "        list       list installed versions"
          , "        uninstall  uninstall the specific version of Python"
          , "        use        select the specific version of Python as cureent version"
          , ""
          , "Flags:"
          , ""
          , "        -config    set an argument as a configure's argument"
          , "        -framework enable framework on macOS"
          , ""
          ]

help :: FilePath -> [String] -> IO ()
help _ [] = putStrLn usage
help _ [topic] = helpOf topic
  where
    helpOf "install" = putStrLn "usage: psla install versions..."
    helpOf "uninstall" = putStrLn "usage: psla uninstall versions..."
    helpOf "use" = putStrLn "usage: psla use version"
    helpOf "list" = putStrLn "usage: psla list"
    helpOf topic = failWith $ "unknown help topic " ++ show topic ++ ". Run 'psla help'."
help _ _ =
  failWith $
    unlines [ "usage: psla help command"
            , ""
            , "Too many arguments given."
            ]

data Flag =
    Config String
  | Framework
    deriving (Eq, Ord, Show)

fromConfig :: Flag -> Maybe String
fromConfig (Config x) = Just x
fromConfig _ = Nothing

installFlags :: State [String] (Maybe Flag)
installFlags = either error id <$> runExceptT (get >>= parse)
  where
    parse :: [String] -> ExceptT String (State [String]) (Maybe Flag)
    parse ("-config" : xs) = do
      when (null xs) $
           throwError "-config: need argument"
      put $ tail xs
      return . Just . Config $ head xs
    parse ("-framework" : xs) = do
      put xs
      return $ Just Framework
    parse _ = return Nothing

parseFlag :: State [String] (Maybe Flag) -> State [String] [Flag]
parseFlag s = s >>= maybe (return []) parseRest
  where
    parseRest :: Flag -> State [String] [Flag]
    parseRest x = (x:) <$> parseFlag s

install :: FilePath -> [String] -> IO ()
install _ [] = failWith "install: 1 or more arguments required"
install root args = do
  let (flags, versions) = runState (parseFlag installFlags) args
  createDirectoryIfMissing True $ root </> "repo"
  mapM_ (clone root) versions
  mapM_ (build root flags) versions

getDest :: FilePath -> String -> FilePath
getDest root version = foldl1 combine [root, "repo", version]

clone :: FilePath -> String -> IO ()
clone root version = do
  exists <- doesDirectoryExist dest
  unless exists $
         callProcess "git" ["clone", "--depth", "1", "--branch", version, repoURI, dest]
  where
    dest :: FilePath
    dest = getDest root version

build :: FilePath -> [Flag] -> String -> IO ()
build root flags version = do
  mapM_ (exec dest)
        [ (dest </> "configure", configOpt ++ frameworkOpt root ++ ["--prefix", foldl1 combine [root, "python", version]])
        , ("make", ["-k", "-j4"])
        , ("make", ["install"])
        ]
  where
    dest :: FilePath
    dest = getDest root version

    configOpt :: [String]
    configOpt = mapMaybe fromConfig flags

    frameworkOpt :: FilePath -> [String]
    frameworkOpt root = ["--enable-framework=" ++ root </> "frameworks" </> version | Framework `elem` flags]

    exec :: FilePath -> (String, [String]) -> IO ()
    exec dest (cmd, args) = do
      (_, _, _, ph) <- createProcess (proc cmd args){ cwd = Just dest }
      code <- waitForProcess ph
      when (code /= ExitSuccess)
           exitFailure

use :: FilePath -> [String] -> IO ()
use _ [] = failWith "use: 1 argument required"
use root [version] = do
  exists <- doesFileExist $ foldl1 combine [root, "python", version, "bin", "python3"]
  unless exists $
         failWith $ "use: not installed: " ++ show version
  createDirectoryIfMissing True $ root </> "bin"
  writeFile dest $ script root version
  perm <- getPermissions dest
  setPermissions dest $ setOwnerExecutable True perm
  where
    dest :: FilePath
    dest = root </> "bin" </> "python"
use _ _ = failWith "use: too many arguments"

script :: String -> String -> String
script root version =
  unlines [ "#!/bin/sh"
          , ""
          , "export PYTHONUSERBASE=" ++ show (root </> "user" </> version)
          , show (foldl1 combine [root, "python", version, "bin", "python3"]) ++ " \"$@\""
          ]

list :: FilePath -> [String] -> IO ()
list root [] = listDirectory (root </> "python") >>= mapM_ putStrLn
list _ _ = failWith "usage: list"

uninstall :: FilePath -> [String] -> IO ()
uninstall _ [] = failWith "usage: psla uninstall versions..."
uninstall root versions =
  mapM_ remove [root </> dir </> v | v <- versions, dir <- ["repo", "python", "frameworks", "user"]]
  where
    remove :: FilePath -> IO ()
    remove = flip catch ignoreNotExist . removeDirectoryRecursive

ignoreNotExist :: IOError -> IO ()
ignoreNotExist = unless . isDoesNotExistError <*> throw
