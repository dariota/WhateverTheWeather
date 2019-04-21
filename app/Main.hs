{-# LANGUAGE OverloadedStrings #-}

module Main where

import DarkSky
import qualified Data.Text as T
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = print "hello"

tempString :: Point -> T.Text
tempString point = T.intercalate " " $ map fromJust $
  if includeBoth actual apparent then
      [actualString, apparentString]
    else
      take 1 $ takeWhile isJust [actualString, apparentString, fallback]
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
