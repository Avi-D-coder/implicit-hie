{-# LANGUAGE CPP, OverloadedStrings, NamedFieldPuns #-}
module TestUtils where

import Data.Aeson.Types (typeMismatch)
import Data.Yaml
import Data.Text
import Data.Typeable

-- |Choose a resolver based on the current compiler, otherwise HaRe/ghc-mod will
-- not be able to load the files
readResolver :: IO String
readResolver = readResolverFrom stackYaml

stackYaml :: FilePath
stackYaml = "stack.yaml"

newtype StackResolver = StackResolver String

instance FromJSON StackResolver where
  parseJSON (Object x) = StackResolver <$> x .: pack "resolver"
  parseJSON invalid = typeMismatch "StackResolver" invalid

readResolverFrom :: FilePath -> IO String
readResolverFrom yamlPath = do
  result <- decodeFileEither yamlPath
  case result of
    Left err -> error $ yamlPath ++ " parsing failed: " ++ show err
    Right (StackResolver res) -> return res
