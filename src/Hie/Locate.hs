module Hie.Locate
  ( nestedPkg,
    nestedCabalFiles,
  )
where

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

nestedCabalFiles :: FilePath -> IO [FilePath]
nestedCabalFiles f = do
  fs <- listDirectory f
  case filter ((".cabal" ==) . takeExtension) fs of
    h : _ -> pure [f </> h]
    _ ->
      fmap concat . mapM nestedCabalFiles
        =<< filterM
          (fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata)
          ( map (f </>) $
              filter
                (`notElem` [".git", "dist", "dist-newstyle", ".stack-work"])
                fs
          )

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
