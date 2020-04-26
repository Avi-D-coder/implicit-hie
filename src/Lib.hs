{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace

type Name = Text

type Path = Text

type Indent = Int

data Component = Lib Path | Exe Name Path | Test Name Path
  deriving (Show, Eq, Ord)

parseComponents :: Parser [Component]
parseComponents =
  ( do
      h <- parseComponent 0
      t <- parseComponents
      pure $ h : t
  )
    <|> (skipToNextLine >> parseComponents)
    <|> (endOfInput >> pure [])

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
    >> Lib <$> parsePath i

parseComponentName :: Parser Name
parseComponentName = do
  skipSpace
  takeWhile1 (not . isSpace)

parseNamed :: Indent -> Text -> (Name -> Path -> Component) -> Parser Component
parseNamed i compType compCon =
  do
    indent i
    _ <- asciiCI compType <?> "asciiCI " <> T.unpack compType
    _ <- skipSpace <?> "skipSpace"
    n <- parseComponentName <?> "N"
    skipToNextLine
    compCon n <$> parsePath i
    <?> T.unpack ("parseNamed " <> compType)

skipToNextLine :: Parser ()
skipToNextLine = skipWhile (not . isEndOfLine) >> endOfLine

parsePath :: Indent -> Parser Path
parsePath i =
  ( do
      indent i
      _ <- "hs-source-dirs"
      skipSpace
      _ <- char ':'
      -- FIXME paths can be in quotes
      p <- parseComponentName
      skipToNextLine
      skipMany $ indent i >> skipToNextLine
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
indent 0 = skipMany space <?> "indent 0"
indent i = space >> indent (i - 1) <?> "indent 0"
