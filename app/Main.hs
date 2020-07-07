{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hie.Cabal.Parser
import Hie.Locate
import Hie.Yaml
import System.Directory
import System.Directory.Internal
import System.FilePath.Posix
import System.Environment

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  name <- resolveName pwd
  cfs <- runMaybeT $ case name of
    "cabal" -> cabalPkgs pwd
    _ -> stackYamlPkgs pwd
  when (null cfs) $ error $
    "No .cabal files found under"
      <> pwd
      <> "\n You may need to run stack build."
  pkgs <- catMaybes <$> mapM (nestedPkg pwd) (concat cfs)
  putStr <$> hieYaml name $ fmtPkgs name pkgs

resolveName :: FilePath -> IO String
resolveName pwd = do
  args <- getArgs
  files <- listDirectory pwd
  let fileNames = map takeFileName files
      name =
        if  | "--cabal" `elem` args -> "cabal"
            | "--stack" `elem` args -> "stack"
            | "dist-newstyle" `elem` fileNames -> "cabal"
            | ".stack-work" `elem` fileNames -> "stack"
            | "cabal.project" `elem` fileNames -> "cabal"
            | "stack.yaml" `elem` fileNames -> "stack"
            | otherwise -> "cabal"
  return name

