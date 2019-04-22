module Console
  (
    Output
  ) where

import qualified Data.Text as T
import qualified System.Console.ANSI.Types as A

data Output =
  Output {
    content :: T.Text,
    fgColour :: A.Color
  } deriving (Show)
