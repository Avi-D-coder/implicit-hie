{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hie.Cabal.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath.Posix ((</>))

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

-- Skip over entire fields that are known to be free-form.  Ensures lines that
-- look like the beginning of sections/stanzas are not inadvertently intepreted
-- as such.
-- List gathered by searching "free text field" in:
-- https://cabal.readthedocs.io/en/3.4/buildinfo-fields-reference.html
-- May be subject to change across Cabal versions.
skipFreeformField :: Parser ()
skipFreeformField =
  choice $
    flip (field 0) skipBlock
      <$> [ "author",
            "bug-reports",
            "category",
            "copyright",
            "description",
            "homepage",
            "maintainer",
            "package-url",
            "stability",
            "synopsis"
          ]

parsePackage :: Parser Package
parsePackage =
  ( do
      n <- field 0 "name" $ const parseString
      (Package _ t) <- parsePackage
      pure $ Package n t
  )
    <|> (skipFreeformField >> parsePackage)
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
parseLib i =
  (parseSec i "library" $ Comp Lib)
    <|> (parseSec i "foreign-library" $ Comp Lib)

parseTestSuite :: Indent -> Parser [Component]
parseTestSuite i = parseSec i "test-suite" $ Comp Test

parseExe :: Indent -> Parser [Component]
parseExe = parseSecMain (Comp Exe) "executable"

parseBench :: Indent -> Parser [Component]
parseBench = parseSecMain (Comp Bench) "benchmark"

parseSecMain :: (Name -> Path -> Component) -> Text -> Indent -> Parser [Component]
parseSecMain c s i = do
  n <- componentHeader i s
  p <- pathMain (i + 1) ["./"] "" [] []
  pure $ map (c n) p

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

pathMain :: Indent -> [Text] -> Text -> [Text] -> [Text] -> Parser [Text]
pathMain i p m o a =
  (hsSourceDir i >>= (\p' -> pathMain i p' m o a))
    <|> (field i "main-is" (const parseString) >>= (\m' -> pathMain i p m' o a))
    <|> (field i "other-modules" parseList >>= flip (pathMain i p m) a)
    <|> (field i "autogen-modules" parseList >>= pathMain i p m o)
    <|> (skipBlockLine i >> pathMain i p m o a)
    <|> pure
      ( map (<//> m) p
          <> [ p' <//> (o'' <> ".hs")
               | p' <- p,
                 o' <- filter (`notElem` a) o,
                 let o'' = T.replace "." "/" o'
             ]
      )

(<//>) :: Text -> Text -> Text
a <//> b = T.pack (T.unpack a </> T.unpack b)

infixr 5 <//>

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
emptyOrComLine = (skipMany tabOrSpace >> endOfLine) <|> comment

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
    skipMany tabOrSpace
    _ <- char ':'
    skipMany tabOrSpace
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
