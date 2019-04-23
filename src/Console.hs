module Console
  (
    Output(..),
    Colour(..),
    unpackOutput
  ) where

import qualified Data.Text as T
import qualified System.Console.ANSI.Types as A

data Output =
  Output {
    content :: T.Text,
    fgColour :: Colour
  } deriving (Show)

data Colour =
  Black | BrightBlack |
    Red | BrightRed |
    Green | BrightGreen |
    Yellow | BrightYellow |
    Blue | BrightBlue |
    Magenta | BrightMagenta |
    Cyan | BrightCyan |
    White | BrightWhite
  deriving (Show)

unpackOutput :: Output -> (T.Text, A.Color, A.ColorIntensity)
unpackOutput output = (content output, colour, intensity)
  where
    (colour, intensity) =
      case fgColour output of
        Black   -> (A.Black, A.Dull)
        Red     -> (A.Red, A.Dull)
        Green   -> (A.Green, A.Dull)
        Yellow  -> (A.Yellow, A.Dull)
        Blue    -> (A.Blue, A.Dull)
        Magenta -> (A.Magenta, A.Dull)
        Cyan    -> (A.Cyan, A.Dull)
        White   -> (A.White, A.Dull)
        BrightBlack   -> (A.Black, A.Vivid)
        BrightRed     -> (A.Red, A.Vivid)
        BrightGreen   -> (A.Green, A.Vivid)
        BrightYellow  -> (A.Yellow, A.Vivid)
        BrightBlue    -> (A.Blue, A.Vivid)
        BrightMagenta -> (A.Magenta, A.Vivid)
        BrightCyan    -> (A.Cyan, A.Vivid)
        BrightWhite   -> (A.White, A.Vivid)
