module Config where

import GHC.Generics
import System.Envy

defaultListenAddress :: Text
defaultListenAddress = "localhost"

defaultListenPort :: Int
defaultListenPort = 8081

data Config = Config
  { listenAddress :: Text,
    listenPort :: Int
  }
  deriving (Generic, Show)

retrieve :: MonadIO m => m (Either String Config)
retrieve =
  liftIO . runEnv $
    gFromEnvCustom
      defOption
      (Just (Config defaultListenAddress defaultListenPort))