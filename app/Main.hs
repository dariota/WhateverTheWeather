{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Console
import DarkSky
import Context
import Data.Maybe (fromJust, isJust, fromMaybe)
import Network.HTTP.Client hiding (Response)
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Text as T (Text, pack, unpack, concat)
import qualified System.Console.ANSI as C
import qualified System.Console.ANSI.Types as CT

main :: IO ()
main = do
  config <- getConfig
  manager <- newManager tlsManagerSettings
  outputs <- runApp (do
                      response <- getCurrentData
                      return $ descriptions response
                    ) $ Context config manager
  case outputs of
    Right output -> mapM_ evaluateOutput output
    Left err     -> print $ "Error encountered: " ++ err

evaluateOutput :: Output -> IO ()
evaluateOutput output = do
  let (content, colour, intensity) = unpackOutput output
  C.setSGR [CT.SetColor CT.Foreground intensity colour]
  putStr $ T.unpack content
  C.setSGR [CT.Reset]

descriptions :: Response -> [Output]
descriptions response = fullString
  where
    tempDescription  = tempString $ currently response
    warnDescription  = alertString $ alerts response
    rainDescription  = precipString $ currently response
    cloudDescription = cloudString $ currently response
    fullString       = concat $ (L.intersperse [commaOut] $ filter (not . null) [tempDescription, cloudDescription, rainDescription, warnDescription]) ++ [[Output "\n" BrightWhite]]

cloudString :: Maybe Point -> [Output]
cloudString Nothing = []
cloudString (Just point) =
  case cloudCover point of
    Just cover -> [coverString cover]
    Nothing    -> []

coverString :: Float -> Output
coverString cover
  | cover > 0.75 = Output "overcast" BrightBlack
  | cover > 0.50 = Output "cloudy" White
  | otherwise    = Output "clear" BrightBlue

precipString :: Maybe Point -> [Output]
precipString Nothing = []
precipString (Just point) =
    if intensity < 0.1 then
      []
    else
      case kind of
        Just precip -> [precipDescription precip intensity]
        Nothing     -> []
  where
    intensity = fromMaybe 0 $ precipIntensity point
    kind = precipType point

precipDescription :: T.Text -> Float -> Output
precipDescription kind intensity = Output (T.concat [kind, " (", T.pack $ show intensity, " mm)"]) colour
  where
    colour =
      case kind of
        "rain"  -> Blue
        "sleet" -> White
        "snow"  -> BrightWhite

tempString :: Maybe Point -> [Output]
tempString Nothing = []
tempString (Just point) = map fromJust $
  if includeBoth actual apparent then
      [actualString, Just $ Output " " BrightWhite, apparentString]
    else
      take 1 $ filter isJust [actualString, apparentString, fallback]
  where
    actual = temperature point
    actualString = fmap (\x -> Output (tempCelsius False x) BrightWhite) actual
    apparent = apparentTemperature point
    apparentString = fmap (\x -> Output (tempCelsius True x) White) apparent
    fallback = Just $ Output (tempCelsius False "?") BrightRed

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

alertString :: Maybe [Alert] -> [Output]
alertString Nothing       = []
alertString (Just [])     = []
alertString (Just alerts) = (Output "[" BrightWhite) : ((L.intersperse commaOut severityTexts) ++ [Output "]" BrightWhite])
  where
    alertTypes = [("advisory", BrightYellow), ("watch", Yellow), ("warning", Red)]
    severityTexts = concat $ map (severityString alerts) alertTypes

severityString :: [Alert] -> (T.Text, Colour) -> [Output]
severityString alerts (kind, colour) =
  if length severities == 0 then
      []
    else
      [Output (T.concat [T.pack . show $ length severities, " ", kind]) colour]
  where
    severities = filter ((== kind) . severity) alerts

commaOut :: Output
commaOut = Output ", " BrightWhite
