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

parsePackage :: Parser Package
parsePackage =
  ( do
      n <- parseName
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

parseComponent :: Indent -> Parser Component
parseComponent i =
  parseLib i
    <|> parseExe i
    <|> parseNamed i "test-suite" Test

parseLib :: Indent -> Parser Component
parseLib i =
  indent i
    >> asciiCI "library"
    >> skipToNextLine
    >> Lib <$> extractPath (i + 1)

parseQuoted :: Parser Text
parseQuoted = do
  q <- char '"' <|> char '\''
  takeTill (== q)

parseString :: Parser Name
parseString = do
  skipSpace
  parseQuoted <|> takeWhile1 (not . (\c -> isSpace c || c == ','))

parseExe :: Indent -> Parser Component
parseExe i =
  do
    indent i
    _ <- asciiCI "executable"
    _ <- skipSpace
    n <- parseString <?> "Exe Name"
    skipToNextLine
    Exe n <$> pathMain (i + 1) "." ""
    <?> T.unpack "parseExe"

pathMain :: Indent -> Text -> Text -> Parser Text
pathMain i p m =
  (field i "hs-source-dirs" >>= (\p' -> pathMain i p' m))
    <|> (field i "main-is" >>= pathMain i p)
    <|> (skipBlockLine i >> pathMain i p m)
    <|> pure (p <> "/" <> m)

parseNamed :: Indent -> Text -> (Name -> Path -> Component) -> Parser Component
parseNamed i compType compCon =
  do
    indent i
    _ <- asciiCI compType <?> "asciiCI " <> T.unpack compType
    _ <- skipSpace <?> "skipSpace"
    n <- parseString <?> "N"
    skipToNextLine
    compCon n <$> extractPath (i + 1)
    <?> T.unpack ("parseNamed " <> compType)

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
