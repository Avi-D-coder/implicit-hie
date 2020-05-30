{-# LANGUAGE LambdaCase #-}
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
import System.FilePath
import Hie.CabalHelper
import Distribution.Helper (Ex (..))

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  projM <- findCabalHelperEntryPoint $ pwd </> "Foo.hs"
  case projM of
    Nothing -> error "could not find project context"
    Just proj@(Ex p) -> do
      env <- initialiseEnvironment p
      packages <- loadPackages env
      pkgs <- toHieYaml env packages
      let name = if | isCabalProject proj -> "cabal"
                    | isStackProject proj -> "stack"
                    | otherwise -> error "Neither stack nor cabal project"
      putStr $ hieYaml name $ fmtPkgs name pkgs

  -- files <- listDirectory pwd
  -- let name =
  --       if  | any (("dist-newstyle" ==) . takeFileName) files -> "cabal"
  --           | any ((".stack-work" ==) . takeFileName) files -> "stack"
  --           | any (("cabal.project" ==) . takeFileName) files -> "cabal"
  --           | any (("stack.yaml" ==) . takeFileName) files -> "stack"
  --           | otherwise -> "cabal"
  -- cfs <- runMaybeT $ case name of
  --   "cabal" -> cabalPkgs pwd
  --   _ -> stackYamlPkgs pwd
  -- when (null cfs) $ error $
  --   "No .cabal files found under"
  --     <> pwd
  --     <> "\n You may need to run stack build."
  -- pkgs <- catMaybes <$> mapM (nestedPkg pwd) (concat cfs)
  -- putStr <$> hieYaml name $ fmtPkgs name pkgs
