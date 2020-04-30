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

data Component = Lib Path | Exe Name Path | Test Name Path
  deriving (Show, Eq, Ord)

parseName :: Parser Text
parseName = "name" >> skipSpace >> char ':' >> parseString

parseSec :: Parser Package
parseSec =
  ( do
      n <- parseName
      (Package _ t) <- parseSec
      pure $ Package n t
  )
    <|> ( do
            h <- parseComponent 0
            (Package n t) <- parseSec
            pure $ Package n (h : t)
        )
    <|> (skipToNextLine >> parseSec)
    <|> pure (Package "" [])

parseComponent :: Indent -> Parser Component
parseComponent i =
  parseLib i
    <|> parseNamed i "executable" Exe
    <|> parseNamed i "test-suite" Test

parseLib :: Indent -> Parser Component
parseLib i =
  indent i
    >> asciiCI "library"
    >> skipToNextLine
    >> Lib <$> parsePath (i + 1)

parseQuoted :: Parser Text
parseQuoted = do
  q <- char '"' <|> char '\''
  takeTill (== q)

parseString :: Parser Name
parseString = do
  skipSpace
  parseQuoted <|> takeWhile1 (not . isSpace)

parseNamed :: Indent -> Text -> (Name -> Path -> Component) -> Parser Component
parseNamed i compType compCon =
  do
    indent i
    _ <- asciiCI compType <?> "asciiCI " <> T.unpack compType
    _ <- skipSpace <?> "skipSpace"
    n <- parseString <?> "N"
    skipToNextLine
    compCon n <$> parsePath (i + 1)
    <?> T.unpack ("parseNamed " <> compType)

skipToNextLine :: Parser ()
skipToNextLine = skipWhile (not . isEndOfLine) >> endOfLine

skipBlock :: Indent -> Parser ()
skipBlock i =
  skipMany $
    (indent i >> skipToNextLine)
      <|> (skipMany tabOrSpace >> endOfLine)
      <|> (skipSpace >> "--" >> skipToNextLine)

tabOrSpace :: Parser Char
tabOrSpace = char ' ' <|> char '\t'

parsePath :: Indent -> Parser Path
parsePath i =
  ( do
      indent i
      _ <- "hs-source-dirs"
      skipSpace
      _ <- char ':'
      -- FIXME paths can be in quotes
      p <- parseString
      skipToNextLine
      skipBlock i
      pure p
      <?> "hs-source-dirs"
  )
    <|> ( do
            indent i
            skipToNextLine
            parsePath i
            <?> "skip line"
        )
    <|> (pure "." <?> "not found") <?> "parsePath"

-- | Skip at least n spaces
indent :: Indent -> Parser ()
indent 0 = skipMany tabOrSpace <?> "indent 0"
indent i = tabOrSpace >> indent (i - 1) <?> "indent 0"
