{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Config
  (
    Config,
      api_key,
      latitude,
      longitude,
    getConfig
  ) where

import Control.Exception
import Data.Aeson
import Data.Maybe
import GHC.Generics
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

data Config =
  Config {
    api_key :: T.Text,
    latitude :: Float,
    longitude :: Float
  } deriving (Show, Generic, Eq)

instance FromJSON Config
instance ToJSON Config

getConfig :: IO Config
getConfig = do
  home <- getHomeDirectory
  file <- B.readFile (configFile home) `catch` handleMissing home
  let maybeConfig = decode file
      config = maybe (error "Could not decode config file") id maybeConfig
  if config == defaultConfig then
    error $ "Please set up your config in " ++ (configFile home)
  else
    return config

handleMissing :: FilePath -> IOException -> IO B.ByteString
handleMissing home e = do
  createDirectoryIfMissing True (configDir home)
  let configJson = encode defaultConfig
  B.writeFile (configFile home) configJson
  return configJson

defaultConfig :: Config
defaultConfig = Config "PLACEHOLDER_API_KEY" 0 0

configDir :: FilePath -> FilePath
configDir home = home ++ "/.wtw/"

configFile :: FilePath -> FilePath
configFile home = (configDir home) ++ "config"
