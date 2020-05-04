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
cabalComponent n (Lib "" p) = comp p $ "lib:" <> n
cabalComponent n (Lib cn p) = comp p $ "lib:" <> n <> ":" <> cn
cabalComponent n (Exe cn p) = comp p $ n <> ":exe:" <> cn
cabalComponent n (Test cn p) = comp p $ n <> ":test:" <> cn

stackComponent :: Name -> Component -> T.Text
stackComponent n (Lib "" p) = comp p $ n <> ":lib"
stackComponent n (Lib cn p) = comp p $ n <> ":lib:" <> cn
stackComponent n (Exe cn p) = comp p $ n <> ":exe:" <> cn
stackComponent n (Test cn p) = comp p $ n <> ":test:" <> cn

comp :: T.Text -> T.Text -> T.Text
comp p c =
  "- path: "
    <> dQuote p
    <> "\n  "
    <> "component: "
    <> dQuote c

dQuote :: T.Text -> T.Text
dQuote t = T.cons '"' t `T.snoc` '"'
