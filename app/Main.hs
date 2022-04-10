{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
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
  name <- resolveName pwd
  cfs <- runMaybeT $ case name of
    "cabal" -> cabalPkgs pwd
    _ -> stackYamlPkgs pwd
  when (null cfs) $
    die $
      "Used " <> name
        <> "\n No .cabal files found under"
        <> pwd
        <> "\n You may need to run stack build."
  pkgs <- catMaybes <$> mapM (nestedPkg pwd) (concat cfs)
  putStr <$> hieYaml name $ fmtPkgs name pkgs

resolveName :: FilePath -> IO String
resolveName pwd = do
  args <- getArgs
  files <- listDirectory pwd
  when ("--help" `elem` args || "-h" `elem` args) $ do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " <> progName <> " [ --cabal | --stack ]\n\n\
      \If neither argument is given then " <> progName <> " will infer the type by\n\
      \looking for dist-newstyle, .stack-work, cabal.project and stack.yaml in that order."
    exitSuccess
  let fileNames = map takeFileName files
      name =
        if
            | "--cabal" `elem` args -> "cabal"
            | "--stack" `elem` args -> "stack"
            | "dist-newstyle" `elem` fileNames -> "cabal"
            | ".stack-work" `elem` fileNames -> "stack"
            | "cabal.project" `elem` fileNames -> "cabal"
            | "stack.yaml" `elem` fileNames -> "stack"
            | otherwise -> "cabal"
  return name
