{-# LANGUAGE DeriveGeneric #-}

module DarkSky
  (
    Response,
      latitude,
      longitude,
    Alert,
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text

data Response =
  Response {
    latitude :: Float,
    longitude :: Float,
    timezone :: Text,
    alerts :: Maybe [Alert]
  } deriving (Show, Generic)

instance FromJSON Response

data Alert =
  Alert {
    description :: Text,
    expires :: Int,
    regions :: [Text],
    severity :: Text,
    time :: Int,
    title :: Int,
    uri :: Text
  } deriving (Show, Generic)

instance FromJSON Alert
