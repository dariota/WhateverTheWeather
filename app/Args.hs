module Args
  (
    withOpts,
    Program
  ) where

import System.Environment (getProgName)

data Options = AlertOpts | WeatherOpts
  deriving (Show)

type Program = (Options -> IO ())

withOpts :: Program -> Program -> IO ()
withOpts weatherProg alertProg = do
  opts <- getOpts
  case opts of
    AlertOpts   -> alertProg opts
    WeatherOpts -> weatherProg opts

getOpts :: IO Options
getOpts = do
  name <- getProgName
  return $ case name of
             "alerts"  -> AlertOpts
             "weather" -> WeatherOpts
             otherwise -> error $ "Unrecognised program name " ++ name
