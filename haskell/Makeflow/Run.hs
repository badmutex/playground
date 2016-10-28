
module Makeflow.Run where

import Makeflow.Monad
import Makeflow.Workflow

import HSH
import System.FilePath
import System.Directory


buildSandbox :: FilePath -> Workflow -> IO ()
buildSandbox path workflow = do
  createDirectoryIfMissing True path
  writeFile (path</>"Makeflow") (makeflow workflow)

executeSandbox :: FilePath -> IO ()
executeSandbox path = do
  cd path
  run "makeflow"

cleanSandbox :: FilePath -> IO ()
cleanSandbox path = do
  cd path
  run "makeflow -c"
