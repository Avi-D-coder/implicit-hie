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
      traceM "do"
      h <- parseComponent 0
      t <- parseComponents
      pure $ h : t
  )
    <|> (traceM "skip" >> skipToNextLine >> parseComponents)
    <|> (traceM "pure" >> pure [])

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
      p <- parseComponentName
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
