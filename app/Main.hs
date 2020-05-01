{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hie.Cabal.Parser
import Hie.Yaml
import System.Directory
import System.FilePath.Posix

main :: IO ()
main = do
  files <- listDirectory =<< getCurrentDirectory
  let path = filter ((".cabal" ==) . takeExtension) files
      sOrC =
        if  | any ((".stack-work" ==) . takeFileName) files -> stackHieYaml
            | any (("dist-newstyle" ==) . takeFileName) files -> cabalHieYaml
            | otherwise -> stackHieYaml
  when (null path) $ error "No .cabal file found!\n You may need to run stack build."
  file <- T.readFile $ head path
  case parseOnly parsePackage file of
    Right r -> T.writeFile "hie.yaml" $ sOrC r
    _ -> error "Could not parse *.cabal file"
