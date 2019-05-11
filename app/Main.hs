{-# LANGUAGE OverloadedStrings #-}

module Main where

import Args
import Config
import Console
import Context
import DarkSky
import Data.Maybe
import Generator
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = withOpts weatherProgram alertProgram

weatherProgram :: Program
weatherProgram opts = do
  config <- getConfig
  manager <- newManager tlsManagerSettings
  outputs <- runApp (do
                      response <- getCurrentData
                      return $ weatherString response
                    ) $ Context config manager
  case outputs of
    Right output -> putStr $ concat $ map evaluateOutput output
    Left err     -> putStrLn $ "Error encountered: " ++ err

alertProgram :: Program
alertProgram opts = do
  putStrLn "alert time"
