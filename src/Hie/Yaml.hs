{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hie.Yaml
  ( hieYaml,
    fmtComponent,
    fmtPkgs,
    cabalComponent,
    stackComponent,
    component,
  )
where

import qualified Data.Text as T
import Hie.Cabal.Parser

hieYaml :: CradleType -> String -> String
hieYaml ct pkgs =
  "cradle:\n"
    <> indent'
      (cradleTypeName ct <> ":\n" <> indent' pkgs)

indent' :: String -> String
indent' =
  unlines
    . map
      ( \l -> case l of
          "" -> ""
          _ -> "  " <> l
      )
    . lines

component :: CradleType -> Name -> Component -> (FilePath, String)
component = \case
  CabalCradle -> cabalComponent
  StackCradle -> stackComponent

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

-- | Same as init but handle empty list without throwing errors.
dropLast :: [a] -> [a]
dropLast l = take (length l - 1) l

fmtPkgs :: CradleType -> [Package] -> String
fmtPkgs ct pkgs = dropLast $ unlines l
  where
    f (Package n cs) = map ((<> "\n") . fmtComponent . component ct n) cs
    l = concatMap f pkgs

dQuote :: String -> String
dQuote t = '"' : t <> "\""
