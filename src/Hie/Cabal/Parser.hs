{-# LANGUAGE OverloadedStrings #-}

module Hie.Cabal.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

type Name = Text

type Path = Text

type Indent = Int

data Package = Package Name [Component]
  deriving (Show, Eq, Ord)

data Component = Lib Name Path | Exe Name Path | Test Name Path
  deriving (Show, Eq, Ord)

parsePackage :: Parser Package
parsePackage =
  ( do
      n <- field 0 "name"
      (Package _ t) <- parsePackage
      pure $ Package n t
  )
    <|> ( do
            h <- parseComponent 0
            (Package n t) <- parsePackage
            pure $ Package n (h : t)
        )
    <|> (skipToNextLine >> parsePackage)
    <|> pure (Package "" [])

componentHeader :: Indent -> Text -> Parser Name
componentHeader i t = do
  indent i
  _ <- asciiCI t
  skipMany tabOrSpace
  n <- parseString <|> pure ""
  skipToNextLine
  pure n

parseComponent :: Indent -> Parser Component
parseComponent i =
  parseExe i
    <|> parseLib i
    <|> parseTestSuite i

parseLib :: Indent -> Parser Component
parseLib i = parseSec i "library" Lib

parseTestSuite :: Indent -> Parser Component
parseTestSuite i = parseSec i "test-suite" Test

parseExe :: Indent -> Parser Component
parseExe i = do
  n <- componentHeader i "executable"
  Exe n <$> pathMain (i + 1) "." ""

parseQuoted :: Parser Text
parseQuoted = do
  q <- char '"' <|> char '\''
  takeTill (== q)

parseString :: Parser Name
parseString = parseQuoted <|> takeWhile1 (not . (\c -> isSpace c || c == ','))

pathMain :: Indent -> Text -> Text -> Parser Text
pathMain i p m =
  (field i "hs-source-dirs" >>= (\p' -> pathMain i p' m))
    <|> (field i "main-is" >>= pathMain i p)
    <|> (skipBlockLine i >> pathMain i p m)
    <|> pure (p <> "/" <> m)

parseSec :: Indent -> Text -> (Name -> Path -> Component) -> Parser Component
parseSec i compType compCon = do
  n <- componentHeader i compType
  compCon n <$> extractPath (i + 1)

skipToNextLine :: Parser ()
skipToNextLine = skipWhile (not . isEndOfLine) >> endOfLine

skipBlock :: Indent -> Parser ()
skipBlock i = skipMany $ skipBlockLine i

skipBlockLine :: Indent -> Parser ()
skipBlockLine i =
  (indent i >> skipToNextLine)
    <|> (skipMany tabOrSpace >> endOfLine)
    <|> (skipSpace >> "--" >> skipToNextLine)

tabOrSpace :: Parser Char
tabOrSpace = char ' ' <|> char '\t'

field :: Indent -> Text -> Parser Text
field i f =
  do
    indent i
    _ <- asciiCI f
    skipSpace
    _ <- char ':'
    skipSpace
    p <- parseString
    skipToNextLine
    pure p

parseMainIs :: Indent -> Parser Path
parseMainIs i =
  do
    p <- field i "main-is"
    skipBlock i
    pure p
    <?> "hs-source-dirs"

extractPath :: Indent -> Parser Path
extractPath i =
  ( do
      p <- field i "hs-source-dirs"
      skipBlock i
      pure p
  )
    <|> (skipBlockLine i >> extractPath i <?> "skip line")
    <|> (pure "." <?> "not found") <?> "extractPath"

-- | Skip at least n spaces
indent :: Indent -> Parser ()
indent 0 = skipMany tabOrSpace <?> "indent 0"
indent i = tabOrSpace >> indent (i - 1) <?> "indent 0"
