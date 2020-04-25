module Main where

import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- T.readFile $ head args
  case parseOnly parseComponents file of
    Right r -> do
      putStrLn $ show (length r) <> " components"
      mapM_ print r
    _ -> error "Could not parse *.cabal file"
