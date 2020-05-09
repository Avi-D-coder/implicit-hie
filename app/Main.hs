{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Data.Attoparsec.Text
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hie.Cabal.Parser
import Hie.Yaml
import System.Directory
import System.Directory.Internal
import System.FilePath.Posix

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  files <- listDirectory pwd
  cfs <- cabalFiles pwd
  let name =
        if  | any (("dist-newstyle" ==) . takeFileName) files -> "cabal"
            | any ((".stack-work" ==) . takeFileName) files -> "stack"
            | any (("stack.yaml" ==) . takeFileName) files -> "stack"
            | otherwise -> "cabal"
      gen f = do
        f' <- T.readFile f
        case parsePackage' f' of
          Right (Package n cs) -> do
            let hiePath = fst (splitFileName f) </> "hie.yaml"
                dir =
                  fromJust $ stripPrefix (splitDirectories pwd)
                    $ splitDirectories
                    $ fst (splitFileName f)
                pkg =
                  Package n $
                    map
                      ( \(Comp t n p) ->
                          Comp t n (T.pack $ joinPath dir </> T.unpack p)
                      )
                      cs
            pure $ Just pkg
          _ -> pure Nothing
  when (null cfs) $ error $
    "No .cabal files found under"
      <> pwd
      <> "\n You may need to run stack build."
  pkgs <- catMaybes <$> mapM gen cfs
  putStr <$> hieYaml name $ fmtPkgs name pkgs

cabalFiles :: FilePath -> IO [FilePath]
cabalFiles f = do
  fs <- listDirectory f
  case filter ((".cabal" ==) . takeExtension) fs of
    h : _ -> pure [f </> h]
    _ ->
      fmap concat . mapM cabalFiles
        =<< filterM
          (fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata)
          ( map (f </>) $
              filter
                (`notElem` [".git", "dist", "dist-newstyle", ".stack-work"])
                fs
          )
