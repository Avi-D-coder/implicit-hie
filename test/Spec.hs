{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text.IO as T
import Hie.Cabal.Parser
import Hie.Yaml
import Test.Hspec
import Test.Hspec.Attoparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Should Succeed" $
    it "successfully parses executable section" $
      exeSection ~> parseExe 0
        `shouldParse` [Comp Exe "gen-hie" "app/Main.hs"]
  describe "Should Succeed" $
    it "successfully parses test section" $
      testSection ~> parseTestSuite 0
        `shouldParse` [Comp Test "implicit-hie-test" "test"]
  describe "Should Succeed" $
    it "successfully parses library section" $
      libSection ~> parseLib 0
        `shouldParse` [Comp Lib "" "src"]
  describe "Should Succeed" $
    it "successfully parses library section with 2 hs-source-dirs" $
      libSection2 ~> parseLib 0
        `shouldParse` [Comp Lib "" "src", Comp Lib "" "src2"]
  describe "Should Succeed" $
    it "successfully parses library section with 2 paths under hs-source-dirs" $
      libSection3 ~> parseLib 0
        `shouldParse` [Comp Lib "" "src", Comp Lib "" "src2"]
  describe "Should Succeed" $
    it "successfully parses bench section" $
      do
        bs <- T.readFile "test/benchSection"
        bs ~> parseBench 0
          `shouldParse` [Comp Bench "folds" "benchmarks/folds.hs"]
  describe "Should Succeed" $
    it "successfully parses package" $
      do
        cf <- T.readFile "implicit-hie.cabal"
        cf ~> parsePackage
          `shouldParse` Package
            "implicit-hie"
            [ Comp Lib "" "src",
              Comp Exe "gen-hie" "app/Main.hs",
              Comp Test "implicit-hie-test" "test"
            ]
  describe "Should Succeed" $
    it
      "skips to end of block section"
      $ let r = "test\n"
         in (libSection <> r) ~?> parseLib 0
              `leavesUnconsumed` r
  describe "Should Succeed" $
    it "successfully generates stack hie.yaml" $
      do
        sf <- readFile "test/stackHie.yaml"
        cf <- T.readFile "implicit-hie.cabal"
        (hieYaml "stack" . fmtPkgs "stack" . (: []) <$> parseOnly parsePackage cf)
          `shouldBe` Right sf
  describe "Should Succeed" $
    it "successfully generates cabal hie.yaml for haskell-language-server" $
      do
        f <- T.readFile "test/haskell-language-server-cabal"
        o <- readFile "test/hie.yaml.cbl"
        (hieYaml "cabal" . fmtPkgs "cabal" . (: []) <$> parseOnly parsePackage f)
          `shouldBe` Right o
  describe "Should Succeed" $
    it "successfully parses comma list" $
      ("one, two" :: Text) ~> parseList 1 `shouldParse` ["one", "two"]
  describe "Should Succeed" $
    it "successfully parses newline list" $
      ("one\n two \n three3" :: Text) ~> parseList 1
        `shouldParse` ["one", "two", "three3"]
  describe "Should Succeed" $
    it "successfully parses newline comma list" $
      ("one\n two,  three3" :: Text) ~> parseList 1
        `shouldParse` ["one", "two", "three3"]
  describe "Should Succeed" $
    it "quoted list" $
      ("\"one\"\n two\n three3" :: Text) ~> parseList 1
        `shouldParse` ["one", "two", "three3"]
  describe "Should Succeed" $
    it "list with leading commas" $
      ("one\n , two\n , three3" :: Text) ~> parseList 1
        `shouldParse` ["one", "two", "three3"]
  describe "Should Succeed" $
    it "list with a comment" $
      ("foo\n  -- need to include this too\n  bar\n" :: Text) ~> parseList 1
        `shouldParse` ["foo", "bar"]
  describe "Should Succeed" $
    it "list2 with a comment" $
      ("foo  -- need to include this too\n  bar\n" :: Text) ~> parseList 1
        `shouldParse` ["foo", "bar"]
  describe "Should Succeed" $
    it "list3 with a comment" $
      ("foo  -- need to include this too\n  bar" :: Text) ~> parseList 1
        `shouldParse` ["foo", "bar"]
  describe "Should Succeed" $
    it "list4 with a comment" $
      ("foo\n bar\n  -- need to include this too" :: Text) ~> parseList 1
        `shouldParse` ["foo", "bar"]
  describe "Should Succeed" $
    it "list5 with a comment" $
      ("foo\n bar -- need to include this too" :: Text) ~> parseList 1
        `shouldParse` ["foo", "bar"]
  describe "Should Succeed" $
    it "succesfully parses exe component with other-modules containing dots" $
      exeSection2 ~> parseExe 0
        `shouldParse` [ Comp Exe "gen-hie" "app/Main.hs",
                        Comp Exe "gen-hie" "app/Hie/Executable/Helper.hs",
                        Comp Exe "gen-hie" "app/Hie/Executable/Utils.hs"
                      ]
  describe "Should Succeed" $
    it "succesfully parses single other-modules" $
      ("other-modules: test\ndefault-language:   Haskell2011" :: Text) ~?> field 0 "other-modules" parseList
        `leavesUnconsumed` "default-language:   Haskell2011"
  describe "Should Succeed" $
    it "succesfully parses empty other-modules1" $
      ("other-modules: test\ndefault-language:   Haskell2011" :: Text) ~?> field 0 "other-modules" parseList
        `leavesUnconsumed` "default-language:   Haskell2011"
  describe "Should Succeed" $
    it "succesfully parses empty other-modules2" $
      ("    other-modules: \n    build-depends:\n        base >=4.9 && <5" :: Text) ~> field 0 "other-modules" parseList
        `shouldParse` []

exeSection :: Text
exeSection =
  "executable gen-hie\n\
  \  other-modules:\n\
  \    Paths_implicit_hie\n\
  \  autogen-modules:\n\
  \    Paths_implicit_hie\n\
  \  hs-source-dirs:\n\
  \      app\n\
  \  ghc-options: -O2\n\
  \  main-is: Main.hs \n"

testSection :: Text
testSection =
  "test-suite implicit-hie-test\n\
  \  type: exitcode-stdio-1.0\n\
  \  other-modules:\n\
  \      Paths_implicit_hie\n\
  \  hs-source-dirs:\n\
  \      test\n\
  \  ghc-options: -fspecialize-aggressively -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-name-shadowing -fwarn-redundant-constraints -threaded -rtsopts -with-rtsopts=-N\n\
  \  main-is: Spec.hs\n\
  \  build-depends:\n\
  \      attoparsec\n\
  \    , base >=4.7 && <5\n\
  \    , hspec\n\
  \    , hspec-attoparsec\n\
  \    , implicit-hie\n\
  \    , text\n\
  \  default-language: Haskell2010\n"

libSection :: Text
libSection =
  "library\n\
  \  exposed-modules:\n\
  \  Lib\n\
  \  other-modules:\n\
  \    Paths_implicit_hie\n\
  \  hs-source-dirs:\n\
  \    src\n\
  \  ghc-options: -fspecialize-aggressively -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-name-shadowing -fwarn-redundant-constraints\n\
  \  build-depends:\n\
  \    attoparsec\n\
  \  , base >=4.7 && <5\n\
  \  , text\n\
  \  default-language: Haskell2010\n\
  \"

libSection2 :: Text
libSection2 =
  "library\n\
  \  exposed-modules:\n\
  \  Lib\n\
  \  other-modules:\n\
  \  Paths_implicit_hie\n\
  \  hs-source-dirs:\n\
  \    src\n\
  \  hs-source-dirs:\n\
  \   src2\n\
  \  ghc-options: -fspecialize-aggressively -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-name-shadowing -fwarn-redundant-constraints\n\
  \  build-depends:\n\
  \  attoparsec\n\
  \  , base >=4.7 && <5\n\
  \  , text\n\
  \  default-language: Haskell2010\n\
  \"

libSection3 :: Text
libSection3 =
  "library\n\
  \  exposed-modules:\n\
  \  Lib\n\
  \  other-modules:\n\
  \  Paths_implicit_hie\n\
  \  hs-source-dirs:\n\
  \   src,\n\
  \   src2\n\
  \  ghc-options: -fspecialize-aggressively -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-name-shadowing -fwarn-redundant-constraints\n\
  \  build-depends:\n\
  \  attoparsec\n\
  \  , base >=4.7 && <5\n\
  \  , text\n\
  \  default-language: Haskell2010\n\
  \"

exeSection2 :: Text
exeSection2 =
  "executable gen-hie\n\
  \  other-modules:\n\
  \    Hie.Executable.Helper\n\
  \    Hie.Executable.Utils\n\
  \  hs-source-dirs:\n\
  \      app\n\
  \  main-is: Main.hs \n"
