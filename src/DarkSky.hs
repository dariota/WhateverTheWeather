{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module DarkSky
  (
    Response,
      alerts,
      currently,
    Point,
      apparentTemperature,
      cloudCover,
      precipIntensity,
      precipType,
      temperature,
    Alert,
      severity,
    getCurrentData
  ) where

import Context
import Data.Aeson
import Data.ByteString.UTF8 hiding (decode)
import Data.Maybe
import Data.Text
import GHC.Generics
import Network.HTTP.Types
import qualified Config as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Network.HTTP.Client as N

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

getCurrentData :: App Response
getCurrentData = do
  request <- buildRequest
  manager <- asks httpManager
  response <- liftIO $ N.httpLbs request manager
  parseResponse response

parseResponse :: N.Response Lazy.ByteString -> App Response
parseResponse response = do
  let status = N.responseStatus response
      code = statusCode status
      parsed = decode (N.responseBody response)
  if code /= 200 then
    throwError $ Prelude.concat ["Error: ", toString $ statusMessage status, ", code: ", show $ code]
  else
    if isJust parsed then
       return $ fromJust parsed
    else
      throwError "parse error"

buildRequest :: App N.Request
buildRequest = do
  config <- asks config
  let key = fromString $ unpack $ C.api_key config
      lat = fromString $ show $ C.latitude config
      lon = fromString $ show $ C.longitude config
      request = baseRequest {
                  N.path = B.concat [basePath, "/", key, "/", lon, ",", lat],
                  N.queryString = defaultParams
                }
  return request

baseRequest :: N.Request
baseRequest = N.defaultRequest {
                N.secure = True,
                N.port = 443,
                N.host = baseHost,
                N.method = "GET"
              }

baseHost :: B.ByteString
baseHost = "api.darksky.net"

basePath :: B.ByteString
basePath = "forecast"

defaultParams :: B.ByteString
defaultParams = "exclude=minutely,hourly,daily,flags&units=si"
