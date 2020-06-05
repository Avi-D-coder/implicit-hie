{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Hie.Locate
  ( stackYamlPkgs,
    cabalPkgs,
  )
where

import           Cabal.Project
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Attoparsec.Text                         (parseOnly)
import           Data.Bifunctor
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Text                                    as T
import qualified Data.Text.IO                                 as T
import           Data.Yaml
import           Distribution.Pretty
import           Distribution.Types.BuildInfo
import           Distribution.Types.CondTree
import           Distribution.Types.Executable
import           Distribution.Types.GenericPackageDescription
import           Distribution.Types.Library
import           Distribution.Types.LibraryName
import           Distribution.Types.PackageDescription
import           Distribution.Types.PackageId
import           Distribution.Types.PackageName
import           Distribution.Types.TestSuite
import           Distribution.Types.TestSuiteInterface
import           GHC.Generics
import           Hie.Cabal.Parser                             (CompType (..),
                                                               Component (..),
                                                               Package (..))
import           Hie.Yaml
import           System.Directory
import           System.FilePath.Posix
import           System.FilePattern.Directory                 (getDirectoryFiles)

newtype Pkgs = Pkgs [FilePath]
  deriving (Eq, Ord)

instance FromJSON Pkgs where
  parseJSON (Object v) = Pkgs <$> v .: "packages"
  parseJSON _          = fail "could not read packages from stack.yaml"

stackYamlPkgs :: FilePath -> MaybeT IO [FilePath]
stackYamlPkgs p = liftIO $
  decodeFileEither (p </> "stack.yaml") >>= \case
    Right (Pkgs f) ->
      liftIO $
        map (p </>)
          <$> getDirectoryFiles p (map (</> "*.cabal") f)
    Left e -> fail $ show e

cabalPkgs :: FilePath -> MaybeT IO [Package]
cabalPkgs p = do
  cp <- cabalP "cabal.project"
  cl <- cabalP "cabal.project.local"
  case rights [cp, cl] of
    -- FIXME parse cabal files w/o project file
    --[] -> liftIO (cfs p) >>= \case
      --[] -> fail "no cabal files found"
      --h : _ -> pure [p </> h]
    xs ->
      fmap concat $ forM xs $ \Project{..} ->
        forM prjPackages $ \(pkgPath, pkg) -> do
          let pkgDir = takeDirectory pkgPath
          let pkgDescr = packageDescription pkg
          let name = T.pack $ unPackageName . pkgName . package $ pkgDescr
          let lib = case condLibrary pkg of
                Nothing -> []
                Just lib -> Comp Lib (libNameToText $ libName $ condTreeData lib) . T.pack . (pkgDir </> ) <$> hsSourceDirs (libBuildInfo $ condTreeData lib)
          let exes = concat $ exeToComponent pkgDir . condTreeData . snd <$> condExecutables pkg
          let tests = concat $ uncurry (testSuiteToComponent pkgDir) . second condTreeData <$> condTestSuites pkg
          return $ Package name $ lib <> exes <> tests
  where
    libNameToText LMainLibName          = ""
    libNameToText (LSubLibName libName) = T.pack $ prettyShow libName

    exeToComponent pkgDir exe = Comp Exe (T.pack $ prettyShow $ exeName exe) <$>
      (T.pack . (</> modulePath exe) . (pkgDir </>) <$> hsSourceDirs (buildInfo exe)) -- TODO other-modules autogen-modules

    testSuiteToComponent pkgDir name testSuite = Comp Test (T.pack $ prettyShow name) . T.pack . (pkgDir </>) <$>
      hsSourceDirs (testBuildInfo testSuite)

    testSuiteInterfaceToPath (TestSuiteExeV10 _ p) = p
    testSuiteInterfaceToPath i = error $ "Unsupported test suite " <> show i

    cabalP n = liftIO (try @IOException $ readProject $ p </> n)
    cfs d = filter ((".cabal" ==) . takeExtension) <$> listDirectory d
    matchDirs "." = "./*.cabal"
    matchDirs p | "/" `isSuffixOf` p || p == "." = p <> "*.cabal"
    matchDirs p | "*" `isSuffixOf` p || takeExtension p == "" = p <> "/*.cabal"
    matchDirs p = p
