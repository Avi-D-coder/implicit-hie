{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hie.Locate
  ( nestedPkg,
    nestedCabalFiles,
    stackYamlPkgs,
    cabalProjectPkgs,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml
import GHC.Generics
import Hie.Cabal.Parser
import Hie.Yaml
import System.Directory
import System.Directory.Internal
import System.FilePath.Posix

newtype Pkgs = Pkgs [FilePath]
  deriving (Eq, Ord)

instance FromJSON Pkgs where
  parseJSON (Object v) = Pkgs <$> v .: "packages"
  parseJSON _ = fail "could not read packages from stack.yaml"

stackYamlPkgs :: FilePath -> MaybeT IO [FilePath]
stackYamlPkgs p = liftIO $
  decodeFileEither (p </> "stack.yaml") >>= \case
    Right (Pkgs f) -> pure f
    Left e -> fail $ show e

cabalProjectPkgs :: FilePath -> MaybeT IO [FilePath]
cabalProjectPkgs p = do
  cp <- liftIO $ T.readFile $ p </> "cabal.project"
  case parseOnly extractPkgs cp of
    Right f -> pure $ map T.unpack f
    _ -> fail "No packages found"

nestedCabalFiles :: FilePath -> IO [FilePath]
nestedCabalFiles f = do
  fs <- listDirectory f
  nf <-
    fmap concat . mapM nestedCabalFiles
      =<< filterM
        (fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata)
        ( map (f </>) $
            filter
              (`notElem` [".git", "dist", "dist-newstyle", ".stack-work"])
              fs
        )
  let cf = filter ((".cabal" ==) . takeExtension) fs
  pure $ map (f </>) cf <> nf

nestedPkg :: FilePath -> FilePath -> IO (Maybe Package)
nestedPkg parrent child = do
  f' <- T.readFile child
  case parsePackage' f' of
    Right (Package n cs) -> do
      let dir =
            fromJust $ stripPrefix (splitDirectories parrent)
              $ splitDirectories
              $ fst (splitFileName child)
          pkg =
            Package n $
              map
                ( \(Comp t n p) ->
                    Comp t n (T.pack $ joinPath dir </> T.unpack p)
                )
                cs
      pure $ Just pkg
    _ -> pure Nothing
