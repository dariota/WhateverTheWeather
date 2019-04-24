module Context
  (
    Context(..),
    App,
    runApp,
    throwError,
    asks,
    liftIO
  ) where

import Config
import Control.Monad.Except
import Control.Monad.Reader
import Network.HTTP.Client

data Context =
  Context {
    config :: Config,
    httpManager :: Manager
  }

type App = ReaderT Context (ExceptT String IO)

runApp :: App a -> Context -> IO (Either String a)
runApp x a = runExceptT (runReaderT x a)
