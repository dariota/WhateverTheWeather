{-# LANGUAGE OverloadedStrings #-}

module Main where

import DarkSky
import qualified Data.Text as T

main :: IO ()
main = print "hello"

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
