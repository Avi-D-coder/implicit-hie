{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hie.Cabal.Parser
  ( Package (..),
    Component (..),
    CompType (..),
    Name,
    extractPkgs,
    parsePackage',
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Foldable (asum)
import Data.Maybe
  ( catMaybes,
    maybeToList,
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Distribution.ModuleName
  ( ModuleName,
    toFilePath,
  )
import Distribution.Package
  ( pkgName,
    unPackageName,
  )
import Distribution.PackageDescription
  ( Benchmark (benchmarkBuildInfo, benchmarkInterface, benchmarkName),
    BenchmarkInterface (BenchmarkExeV10),
    Executable (buildInfo, exeName, modulePath),
    ForeignLib (foreignLibBuildInfo, foreignLibName),
    Library (libBuildInfo, libName),
    LibraryName (..),
    TestSuiteInterface (TestSuiteExeV10),
    benchmarkModules,
    exeModules,
    explicitLibModules,
    foreignLibModules,
  )
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec
import Distribution.Types.BuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.TestSuite
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path (getSymbolicPath)
import GHC.IO (unsafePerformIO)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

type Name = Text

type Path = Text

type Indent = Int

data Package = Package Name [Component]
  deriving (Show, Eq, Ord)

data CompType = Lib | Exe | Test | Bench
  deriving (Show, Eq, Ord)

data Component
  = Comp CompType Name Path
  deriving (Show, Eq, Ord)

parseQuoted :: Parser Text
parseQuoted = do
  q <- char '"' <|> char '\''
  s <- takeTill (== q)
  _ <- char q
  pure s

parseString :: Parser Name
parseString = parseQuoted <|> unqualName

unqualName :: Parser Text
unqualName = takeWhile1 (not . (\c -> isSpace c || c == ','))

-- | Comma or space separated list, with optional new lines.
parseList :: Indent -> Parser [Text]
parseList i = many (nl <|> sl)
  where
    sep = skipMany (char ',' <|> tabOrSpace)
    com = skipMany tabOrSpace >> "--" >> skipWhile (not . isEndOfLine)
    sl = do
      sep
      x <- parseString
      sep
      skipMany com
      pure x

    nl = do
      skipMany emptyOrComLine <|> endOfLine
      _ <- indent i
      sep
      skipMany com
      x <- parseString
      sep
      skipMany com
      pure x

skipToNextLine :: Parser ()
skipToNextLine = skipWhile (not . isEndOfLine) >> endOfLine

comment :: Parser ()
comment = skipMany tabOrSpace >> "--" >> skipToNextLine

emptyOrComLine :: Parser ()
emptyOrComLine = (skipMany tabOrSpace >> endOfLine) <|> comment

tabOrSpace :: Parser Char
tabOrSpace = char ' ' <|> char '\t'

-- field :: Indent -> Text -> Parser Text
field ::
  Indent ->
  [Text] ->
  (Indent -> Parser a) ->
  Parser a
field i f p =
  do
    i' <- indent i
    _ <- asum $ map asciiCI f
    skipMany tabOrSpace
    _ <- char ':'
    skipMany tabOrSpace
    p' <- p $ i' + 1
    skipToNextLine
    pure p'

-- | Skip at least n spaces
indent :: Indent -> Parser Int
indent i = do
  c <- length <$> many' tabOrSpace
  if c >= i then pure c else fail "insufficient indent"

extractPkgs :: Parser [T.Text]
extractPkgs = join . catMaybes <$> many' (Just <$> field 0 ["packages"] parseList <|> (skipToNextLine >> pure Nothing))

parsePackage' :: T.Text -> Either String Package
parsePackage' t = do
  let bytes = encodeUtf8 t
  case runParseResult (parseGenericPackageDescription bytes) of
    (_warnings, Left err) ->
      error $ "Cannot parse Cabal file: " <> show err
    (_warnings, Right res) -> do
      let pkg = flattenPackageDescription res
      Right $ extractPackage pkg

extractPackage :: PackageDescription -> Package
extractPackage PackageDescription {..} = Package n cc
  where
    n = T.pack . unPackageName $ pkgName package

    cc =
      concat $
        [mkComp Test (unqName $ testName t) (testBuildInfo t) (testExePath t) (testModules t) | t <- testSuites]
          ++ [mkComp Bench (unqName $ benchmarkName b) (benchmarkBuildInfo b) (benchmarkExePath b) (benchmarkModules b) | b <- benchmarks]
          ++ [mkComp Exe (unqName $ exeName e) (buildInfo e) [modulePath e] (exeModules e) | e <- executables]
          ++ [mkComp Lib (libName' l) (libBuildInfo l) [] (explicitLibModules l) | l <- maybeToList library ++ subLibraries]
          ++ [mkComp Lib (unqName $ foreignLibName f) (foreignLibBuildInfo f) [] (foreignLibModules f) | f <- foreignLibs]

    mkComp :: CompType -> T.Text -> BuildInfo -> [FilePath] -> [ModuleName] -> [Component]
    mkComp typ name bi fps mods =
      [ Comp typ name (T.pack fp)
        | fp0 <- fps <> concatMap toFilePath' mods,
          srcDir <- map getSymbolicPath $ hsSourceDirs bi,
          let fp = srcDir </> fp0,
          unsafePerformIO $ doesFileExist fp
      ]

    unqName = T.pack . unUnqualComponentName
    libName' x = case libName x of
      LMainLibName -> ""
      LSubLibName u -> unqName u

benchmarkExePath :: Benchmark -> [FilePath]
benchmarkExePath b = case benchmarkInterface b of
  BenchmarkExeV10 _ f -> [f]
  _ -> []

toFilePath' :: ModuleName -> [FilePath]
toFilePath' mod = [toFilePath mod <.> ext | ext <- ["hs", "lhs"]]

testExePath :: TestSuite -> [FilePath]
testExePath t = case testInterface t of
  TestSuiteExeV10 _ fp -> [fp]
  _ -> []
