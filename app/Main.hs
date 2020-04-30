module Main where

import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import Hie.Cabal.Parser
import Hie.Yaml
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- T.readFile $ head args
  case parseOnly parseSec file of
    Right r -> do
      T.putStr $ cabalHieYaml r
      T.putStr $ stackHieYaml r
    _ -> error "Could not parse *.cabal file"
