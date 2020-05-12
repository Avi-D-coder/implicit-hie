{-# LANGUAGE OverloadedStrings #-}

module Hie.Cabal.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

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

parsePackage' :: Text -> Either String Package
parsePackage' = parseOnly parsePackage

parsePackage :: Parser Package
parsePackage =
  ( do
      n <- field 0 "name" $ const parseString
      (Package _ t) <- parsePackage
      pure $ Package n t
  )
    <|> ( do
            h <- parseComponent 0
            (Package n t) <- parsePackage
            pure $ Package n (h <> t)
        )
    <|> (skipToNextLine >> parsePackage)
    <|> pure (Package "" [])

componentHeader :: Indent -> Text -> Parser Name
componentHeader i t = do
  _ <- indent i
  _ <- asciiCI t
  skipMany tabOrSpace
  n <- parseString <|> pure ""
  skipToNextLine
  pure n

parseComponent :: Indent -> Parser [Component]
parseComponent i =
  parseExe i
    <|> parseLib i
    <|> parseBench i
    <|> parseTestSuite i

parseLib :: Indent -> Parser [Component]
parseLib i = parseSec i "library" $ Comp Lib

parseTestSuite :: Indent -> Parser [Component]
parseTestSuite i = parseSec i "test-suite" $ Comp Test

parseExe :: Indent -> Parser [Component]
parseExe = parseSecMain (Comp Exe) "executable"

parseBench :: Indent -> Parser [Component]
parseBench = parseSecMain (Comp Bench) "benchmark"

parseSecMain :: (Name -> Path -> Component) -> Text -> Indent -> Parser [Component]
parseSecMain c s i = do
  n <- componentHeader i s
  p <- pathMain (i + 1) ["./"] ""
  pure $ map (c n) p

parseQuoted :: Parser Text
parseQuoted = do
  q <- char '"' <|> char '\''
  takeTill (== q)

parseString :: Parser Name
parseString = parseQuoted <|> unqualName

unqualName :: Parser Text
unqualName = takeWhile1 (not . (\c -> isSpace c || c == ','))

parseList :: Indent -> Parser [Text]
parseList i = items <|> (emptyOrComLine >> indent i >> items)
  where
    items = do
      skipMany tabOrSpace
      h <- parseString
      skipMany tabOrSpace
      skipMany (char ',')
      t <-
        items
          <|> (skipToNextLine >> indent i >> parseList i)
          <|> pure []
      pure $ h : t

pathMain :: Indent -> [Text] -> Text -> Parser [Text]
pathMain i p m =
  (hsSourceDir i >>= (\p' -> pathMain i p' m))
    <|> (field i "main-is" (const parseString) >>= pathMain i p)
    <|> (skipBlockLine i >> pathMain i p m)
    <|> pure (map (<> "/" <> m) p)

parseSec :: Indent -> Text -> (Name -> Path -> Component) -> Parser [Component]
parseSec i compType compCon = do
  n <- componentHeader i compType
  p <- extractPath (i + 1) []
  let p' = if null p then ["./"] else p
  pure $ map (compCon n) p'

skipToNextLine :: Parser ()
skipToNextLine = skipWhile (not . isEndOfLine) >> endOfLine

skipBlock :: Indent -> Parser ()
skipBlock i = skipMany $ skipBlockLine i

comment :: Parser ()
comment = skipMany tabOrSpace >> "--" >> skipToNextLine

skipBlockLine :: Indent -> Parser ()
skipBlockLine i = (indent i >> skipToNextLine) <|> emptyOrComLine

emptyOrComLine :: Parser ()
emptyOrComLine = skipMany tabOrSpace >> endOfLine <|> comment

tabOrSpace :: Parser Char
tabOrSpace = char ' ' <|> char '\t'

hsSourceDir :: Indent -> Parser [Text]
hsSourceDir i = field i "hs-source-dirs" parseList

-- field :: Indent -> Text -> Parser Text
field ::
  Indent ->
  Text ->
  (Indent -> Parser a) ->
  Parser a
field i f p =
  do
    i' <- indent i
    _ <- asciiCI f
    skipSpace
    _ <- char ':'
    skipSpace
    p' <- p $ i' + 1
    skipToNextLine
    pure p'

extractPath :: Indent -> [Path] -> Parser [Path]
extractPath i ps =
  (field i "hs-source-dirs" parseList >>= (\p -> extractPath i $ ps <> p))
    <|> (skipBlockLine i >> extractPath i ps)
    <|> (comment >> extractPath i ps)
    <|> pure ps

-- | Skip at least n spaces
indent :: Indent -> Parser Int
indent i = do
  c <- length <$> many' tabOrSpace
  if c >= i then pure c else fail "insufficient indent"

extractPkgs :: Parser [T.Text]
extractPkgs = join . catMaybes <$> many' (Just <$> field 0 "packages" parseList <|> (skipToNextLine >> pure Nothing))
