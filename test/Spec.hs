{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import qualified Data.Text as T
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
  describe "Should Succeed"
    $ it "successfully parses executable section"
    $ exeSection ~> parseExe 0
      `shouldParse` Comp Exe "implicit-hie-exe" "app/Main.hs"
  describe "Should Succeed"
    $ it "successfully parses test section"
    $ testSection ~> parseTestSuite 0
      `shouldParse` Comp Test "implicit-hie-test" "test"
  describe "Should Succeed"
    $ it "successfully parses library section"
    $ libSection ~> parseLib 0
      `shouldParse` Comp Lib "" "src"
  describe "Should Succeed"
    $ it "successfully parses bench section"
    $ benchSection ~> parseBench 0
      `shouldParse` Comp Bench "folds" "benchmarks/folds.hs"
  describe "Should Succeed"
    $ it "successfully parses package"
    $ fullFile ~> parsePackage
      `shouldParse` Package
        "implicit-hie"
        [ Comp Lib "" "src",
          Comp Exe "implicit-hie-exe" "app/Main.hs",
          Comp Test "implicit-hie-test" "test"
        ]
  describe "Should Succeed"
    $ it
      "skips to end of block section"
    $ let r = "test\n"
       in (libSection <> r) ~?> parseLib 0
            `leavesUnconsumed` r
  describe "Should Succeed"
    $ it "successfully generates stack hie.yaml"
    $ (hieYaml "stack" . fmtPkgs "stack" . (: []) <$> parseOnly parsePackage fullFile)
      `shouldBe` Right stackHie
  describe "Should Succeed"
    $ it "successfully generates stack hie.yaml"
    $ do
      f <- T.readFile "test/haskell-language-server-cabal"
      o <- readFile "test/hie.yaml.cbl"
      (hieYaml "cabal" . fmtPkgs "cabal" . (: []) <$> parseOnly parsePackage f)
        `shouldBe` Right o

fullFile :: Text
fullFile = "name: implicit-hie\n" <> libSection <> exeSection <> testSection

exeSection :: Text
exeSection =
  "executable implicit-hie-exe\n\
  \  other-modules:\n\
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
  \  Paths_implicit_hie\n\
  \  hs-source-dirs:\n\
  \  src\n\
  \  ghc-options: -fspecialize-aggressively -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-name-shadowing -fwarn-redundant-constraints\n\
  \  build-depends:\n\
  \  attoparsec\n\
  \  , base >=4.7 && <5\n\
  \  , text\n\
  \  default-language: Haskell2010\n\
  \"

benchSection :: Text
benchSection =
  "benchmark folds\n\
  \  default-language: Haskell2010\n\
  \  hs-source-dirs:   benchmarks\n\
  \  ghc-options:      -Wall -threaded\n\
  \  type:    exitcode-stdio-1.0\n\
  \  main-is: folds.hs\n"

stackHie :: String
stackHie =
  "cradle:\n\
  \  stack:\n\
  \    - path: \"src\"\n\
  \      component: \"implicit-hie:lib\"\n\
  \\n\
  \    - path: \"app/Main.hs\"\n\
  \      component: \"implicit-hie:exe:implicit-hie-exe\"\n\
  \\n\
  \    - path: \"test\"\n\
  \      component: \"implicit-hie:test:implicit-hie-test\"\n\
  \"
