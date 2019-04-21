{-# LANGUAGE DeriveGeneric #-}

module DarkSky
  (
    Response,
      alerts,
    Alert,
      severity
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
    severity :: Text,
    time :: Int,
    title :: Text,
    uri :: Text
  } deriving (Show, Generic)

instance FromJSON Alert
