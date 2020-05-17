{-# LANGUAGE OverloadedStrings #-}

module Hie.Yaml
  ( hieYaml,
    fmtComponent,
    fmtPkgs,
    cabalComponent,
    stackComponent,
  )
where

import qualified Data.Text as T
import Hie.Cabal.Parser

hieYaml :: String -> String -> String
hieYaml sOrC pkgs =
  "cradle:\n"
    <> indent'
      (sOrC <> ":\n" <> indent' pkgs)

indent' :: String -> String
indent' =
  unlines
    . map
      ( \l -> case l of
          "" -> ""
          _ -> "  " <> l
      )
    . lines

cabalComponent :: Name -> Component -> (FilePath, String)
cabalComponent n (Comp Lib "" p) = (T.unpack p, T.unpack $ "lib:" <> n)
cabalComponent n (Comp Lib cn p) = (T.unpack p, T.unpack $ n <> ":lib:" <> cn)
cabalComponent n (Comp Exe cn p) = (T.unpack p, T.unpack $ n <> ":exe:" <> cn)
cabalComponent n (Comp Bench cn p) = (T.unpack p, T.unpack $ n <> ":bench:" <> cn)
cabalComponent n (Comp Test cn p) = (T.unpack p, T.unpack $ n <> ":test:" <> cn)

stackComponent :: Name -> Component -> (FilePath, String)
stackComponent n (Comp Lib "" p) = (T.unpack p, T.unpack $ n <> ":lib")
stackComponent n (Comp Lib cn p) = (T.unpack p, T.unpack $ n <> ":lib:" <> cn)
stackComponent n (Comp Exe cn p) = (T.unpack p, T.unpack $ n <> ":exe:" <> cn)
stackComponent n (Comp Bench cn p) = (T.unpack p, T.unpack $ n <> ":bench:" <> cn)
stackComponent n (Comp Test cn p) = (T.unpack p, T.unpack $ n <> ":test:" <> cn)

fmtComponent :: (FilePath, String) -> String
fmtComponent (p, c) =
  "- path: "
    <> dQuote p
    <> "\n  "
    <> "component: "
    <> dQuote c

dropLast :: [a] -> [a]
dropLast l = take (length l - 1) l

fmtPkgs :: String -> [Package] -> String
fmtPkgs sOrC pkgs = dropLast $ unlines l
  where
    comp = if sOrC == "cabal" then cabalComponent else stackComponent
    f (Package n cs) = map ((<> "\n") . fmtComponent . comp n) cs
    l = concatMap f pkgs

dQuote :: String -> String
dQuote t = '"' : t <> "\""
