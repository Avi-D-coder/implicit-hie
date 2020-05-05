{-# LANGUAGE OverloadedStrings #-}

module Hie.Yaml
  ( cabalHieYaml,
    stackHieYaml,
  )
where

import qualified Data.Text as T
import Hie.Cabal.Parser

cabalHieYaml :: Package -> T.Text
cabalHieYaml (Package n cs) =
  T.pack $
    "cradle:\n"
      <> indent'
        ("cabal:\n" <> indent' (unlines (map (fmtComponent . cabalComponent n) cs)))

stackHieYaml :: Package -> T.Text
stackHieYaml (Package n cs) =
  T.pack $
    "cradle:\n"
      <> indent'
        ("stack:\n" <> indent' (unlines (map (fmtComponent . stackComponent n) cs)))

indent' :: String -> String
indent' = unlines . map ("  " <>) . lines

cabalComponent :: Name -> Component -> (FilePath, String)
cabalComponent n (Lib "" p) = (T.unpack p, T.unpack $ "lib:" <> n)
cabalComponent n (Lib cn p) = (T.unpack p, T.unpack $ "lib:" <> n <> ":" <> cn)
cabalComponent n (Exe cn p) = (T.unpack p, T.unpack $ n <> ":exe:" <> cn)
cabalComponent n (Test cn p) = (T.unpack p, T.unpack $ n <> ":test:" <> cn)

stackComponent :: Name -> Component -> (FilePath, String)
stackComponent n (Lib "" p) = (T.unpack p, T.unpack $ n <> ":lib")
stackComponent n (Lib cn p) = (T.unpack p, T.unpack $ n <> ":lib:" <> cn)
stackComponent n (Exe cn p) = (T.unpack p, T.unpack $ n <> ":exe:" <> cn)
stackComponent n (Test cn p) = (T.unpack p, T.unpack $ n <> ":test:" <> cn)

fmtComponent :: (FilePath, String) -> String
fmtComponent (p, c) =
  "- path: "
    <> dQuote p
    <> "\n  "
    <> "component: "
    <> dQuote c

dQuote :: String -> String
dQuote t = '"' : t <> "\""
