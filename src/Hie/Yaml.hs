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
  "cradle:\n"
    <> indentT ("cabal:\n" <> indentT (T.unlines (map (cabalComponent n) cs)))

stackHieYaml :: Package -> T.Text
stackHieYaml (Package n cs) =
  "cradle:\n"
    <> indentT ("stack:\n" <> indentT (T.unlines (map (stackComponent n) cs)))

indentT :: T.Text -> T.Text
indentT = T.unlines . map ("  " <>) . T.lines

cabalComponent :: Name -> Component -> T.Text
cabalComponent n (Lib p) = component p $ "lib:" <> n
cabalComponent _ (Exe p cn) = component p $ "exe:" <> cn
cabalComponent _ (Test p cn) = component p $ "test:" <> cn

stackComponent :: Name -> Component -> T.Text
stackComponent n (Lib p) = component p $ n <> ":lib"
stackComponent n (Exe cn p) = component p $ n <> ":exe:" <> cn
stackComponent n (Test cn p) = component p $ n <> ":test:" <> cn

component :: T.Text -> T.Text -> T.Text
component p c =
  "- path: "
    <> dQuote p
    <> "\n  "
    <> "component: "
    <> dQuote c

dQuote :: T.Text -> T.Text
dQuote t = T.cons '"' t `T.snoc` '"'
