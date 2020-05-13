{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hie.Locate
  ( nestedPkg,
    stackYamlPkgs,
    cabalPkgs,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text (parseOnly)
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml
import GHC.Generics
import Hie.Cabal.Parser
import Hie.Yaml
import System.Directory
import System.FilePath.Posix
import System.FilePattern.Directory (getDirectoryFiles)

newtype Pkgs = Pkgs [FilePath]
  deriving (Eq, Ord)

instance FromJSON Pkgs where
  parseJSON (Object v) = Pkgs <$> v .: "packages"
  parseJSON _ = fail "could not read packages from stack.yaml"

stackYamlPkgs :: FilePath -> MaybeT IO [FilePath]
stackYamlPkgs p = liftIO $
  decodeFileEither (p </> "stack.yaml") >>= \case
    Right (Pkgs f) ->
      liftIO $
        map (p </>)
          <$> getDirectoryFiles p (map (</> "*.cabal") f)
    Left e -> fail $ show e

cabalPkgs :: FilePath -> MaybeT IO [FilePath]
cabalPkgs p = do
  cp <- cabalP "cabal.project"
  cl <- cabalP "cabal.project.local"
  case concat . rights $ map (parseOnly extractPkgs) $ rights [cp, cl] of
    [] -> liftIO (cfs p) >>= \case
      [] -> fail "no cabal files found"
      h : _ -> pure [p </> h]
    xs -> do
      cd <- liftIO $ map (p </>) <$> getDirectoryFiles p (map (matchDirs . T.unpack) xs)
      cf <-
        liftIO $
          mapM (\p -> if takeExtension p == ".cabal" then pure [p] else cfs p) cd
      pure $ concat cf
  where
    cabalP n = liftIO (try $ T.readFile $ p </> n :: IO (Either IOException T.Text))
    cfs d = filter ((".cabal" ==) . takeExtension) <$> listDirectory d
    matchDirs "." = "./*.cabal"
    matchDirs p | "/" `isSuffixOf` p || p == "." = p <> "*.cabal"
    matchDirs p | "*" `isSuffixOf` p || takeExtension p == "" = p <> "/*.cabal"
    matchDirs p = p

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
