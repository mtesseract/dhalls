{-# LANGUAGE RecordWildCards #-}

module Main where

import App
import Config (Config (..))
import qualified Config
import Control.Exception.Base
import Katip
import qualified Server
import System.IO (stdout)

app :: LogItem c => LogEnv -> c -> Namespace -> Config -> App ()
app logEnv c namespace config@Config {..} = do
  $(logTM) InfoS $ "Started with config: " <> show config
  Server.run logEnv c namespace listenPort

main :: IO ()
main = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "dhalls" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    let initialContext = ()
    let initialNamespace = "main"
    runKatipContextT le initialContext initialNamespace $ do
      Config.retrieve >>= \case
        Left err -> $(logTM) ErrorS $ "Failed to retrieve config: " <> show err <> "; exiting."
        Right config -> app le initialContext initialNamespace config
