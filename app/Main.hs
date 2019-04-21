{-# LANGUAGE OverloadedStrings #-}

module Main where

import DarkSky
import Data.Maybe (fromJust, isJust, fromMaybe, maybe)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson

main :: IO ()
main = do
  json <- B.readFile "test/data/now.json"
  let response        = fromJust $ decode json
      tempDescription = maybe "" tempString $ currently response
      warnDescription = maybe "" alertString $ alerts response
      rainDescription = maybe "" precipString $ currently response
      fullString      = T.intercalate ", " $ filter (/= "") [tempDescription, rainDescription, warnDescription]
  writeFile "output.txt" $ T.unpack fullString

precipString :: Point -> T.Text
precipString point =
    if intensity < 0.1 then
      ""
    else
      case kind of
        Just precip -> T.concat [precip, " (", T.pack $ show intensity, " mm)"]
        Nothing     -> ""
  where
    intensity = fromMaybe 0 $ precipIntensity point
    kind = precipType point

tempString :: Point -> T.Text
tempString point = T.intercalate " " $ map fromJust $
  if includeBoth actual apparent then
      [actualString, apparentString]
    else
      take 1 $ filter isJust [actualString, apparentString, fallback]
  where
    actual = temperature point
    actualString = fmap (tempCelsius False) actual
    apparent = apparentTemperature point
    apparentString = fmap (tempCelsius True) apparent
    fallback = Just $ tempCelsius False "?"

includeBoth :: Maybe Float -> Maybe Float -> Bool
includeBoth (Just a) (Just b) = (abs $ a - b) >= 1
includeBoth _ _ = False

tempCelsius :: (Show a) => Bool -> a -> T.Text
tempCelsius wrap temp = T.concat $
    if wrap then
       ["(", middle, " °C)"]
    else
      [middle, " °C"]
  where
    middle = T.pack . show $ temp

alertString :: [Alert] -> T.Text
alertString []     = ""
alertString alerts = T.concat ["[", T.intercalate ", " $ severityTexts, "]"]
  where
    alertTypes = ["advisory", "watch", "warning"]
    severityTexts = filter (/= "") $ map (severityString alerts) alertTypes

severityString :: [Alert] -> T.Text -> T.Text
severityString alerts kind =
  if length severities == 0 then
      ""
    else
      T.concat [T.pack . show $ length severities, " ", kind]
  where
    severities = filter ((== kind) . severity) alerts
