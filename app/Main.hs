{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import Hie.Cabal.Parser
import Hie.Locate
import Hie.Yaml
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  ct <- resolveCradleType pwd
  cfs <- runMaybeT $ case ct of
    CabalCradle -> cabalPkgs pwd
    StackCradle -> stackYamlPkgs pwd
  when (null cfs) $
    die $
      "Used "
        <> cradleTypeName ct
        <> "\n No .cabal files found under"
        <> pwd
        <> "\n You may need to run stack build."
  pkgs <- catMaybes <$> mapM (nestedPkg pwd) (concat cfs)
  putStr <$> hieYaml ct $ fmtPkgs ct pkgs

resolveCradleType :: FilePath -> IO CradleType
resolveCradleType pwd = do
  args <- getArgs
  files <- listDirectory pwd
  when ("--help" `elem` args || "-h" `elem` args) $ do
    progName <- getProgName
    hPutStrLn stderr $
      "Usage: "
        <> progName
        <> " [ --cabal | --stack ]\n\n\
           \If neither argument is given then "
        <> progName
        <> " will infer the type by\n\
           \looking for dist-newstyle, .stack-work, cabal.project and stack.yaml in that order."
    exitSuccess
  let fileNames = map takeFileName files
      ct =
        if
            | "--cabal" `elem` args -> CabalCradle
            | "--stack" `elem` args -> StackCradle
            | "dist-newstyle" `elem` fileNames -> CabalCradle
            | ".stack-work" `elem` fileNames -> StackCradle
            | "cabal.project" `elem` fileNames -> CabalCradle
            | "stack.yaml" `elem` fileNames -> StackCradle
            | otherwise -> CabalCradle
  return ct
