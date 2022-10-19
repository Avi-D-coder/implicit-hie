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
    it "successfully parses package" $
      do
        cf <- T.readFile "implicit-hie.cabal"
        parsePackage' cf
          `shouldParse` Package
            "implicit-hie"
            [ Comp Test "implicit-hie-test" "test/Spec.hs",
              Comp Exe "gen-hie" "app/Main.hs",
              Comp Lib "" "src/Hie/Cabal/Parser.hs",
              Comp Lib "" "src/Hie/Locate.hs",
              Comp Lib "" "src/Hie/Yaml.hs"
            ]

  describe "Should Succeed" $
    it "successfully generates stack hie.yaml" $
      do
        sf <- readFile "test/stackHie.yaml"
        cf <- T.readFile "implicit-hie.cabal"
        (hieYaml "stack" . fmtPkgs "stack" . (: []) <$> parsePackage' cf)
          `shouldBe` Right sf

  describe "Should Succeed" $
    it "successfully generates cabal hie.yaml for haskell-language-server" $
      do
        f <- T.readFile "test/haskell-language-server-cabal"
        o <- readFile "test/hie.yaml.cbl"
        (hieYaml "cabal" . fmtPkgs "cabal" . (: []) <$> parsePackage' f)
          `shouldBe` Right o
