{-# LANGUAGE DeriveGeneric #-}

module DarkSky
  (
    Response,
      alerts,
      currently,
    Point,
      apparentTemperature,
      temperature,
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
    currently :: Maybe Point,
    alerts :: Maybe [Alert]
  } deriving (Show, Generic)

instance FromJSON Response

data Point =
  Point {
    apparentTemperature :: Maybe Float,
    cloudCover :: Maybe Float,
    dewPoint :: Maybe Float,
    humidity :: Maybe Float,
    icon :: Maybe Text,
    nearestStormBearing :: Maybe Float,
    nearestStormDistance :: Maybe Float,
    ozone :: Maybe Float,
    precipIntensity :: Maybe Float,
    precipIntensityError :: Maybe Float,
    precipProbability :: Maybe Float,
    precipType :: Maybe Text,
    pressure :: Maybe Float,
    summary :: Maybe Text,
    temperature :: Maybe Float,
    uvIndex :: Maybe Integer,
    visibility :: Maybe Float,
    windBearing :: Maybe Float,
    windGust :: Maybe Float,
    windSpeed :: Maybe Float
  } deriving (Show, Generic)

instance FromJSON Point

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
