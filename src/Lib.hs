module Lib
    ( run
    ) where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
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
parseArgs [] = putStrLn usage
parseArgs ("install":args) = install args
parseArgs (x:_) = hPutStrLn stderr $ "psla: no such command " ++ show x

usage :: String
usage = "PSLA is a tool for managing Python environment."

install :: [String] -> IO ()
install [] = hPutStrLn stderr "install: 1 or more arguments required"
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
        void $ waitForProcess ph
        )
