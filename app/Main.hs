module Main where

import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- T.readFile $ head args
  case parseOnly parseSec file of
    Right r -> print r
    _ -> error "Could not parse *.cabal file"
