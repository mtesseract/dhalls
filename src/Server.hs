{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToJSON)
import Data.Aeson.Casing
import qualified Dhall
import Dhall.JSON (dhallToJSON)
import Network.Wai
import qualified Network.Wai.Handler.Warp
import Servant.API
import Servant.Server (Handler, serve, ServerT, hoistServer, err400, ServerError(..))
import Katip
import Control.Exception.Safe (tryAny)
import Servant (throwError)
import Data.Function ((&))

type AppHandler = KatipContextT Handler

data DhallRequest = DhallRequest
  { reqBody :: Text
  }
  deriving (Generic)

data DhallResponse = DhallResponse
  { rspBody :: Value
  }
  deriving (Generic)

instance FromJSON DhallRequest where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON DhallResponse where
  toJSON = genericToJSON $ aesonPrefix snakeCase

type DhallAPI = "dhall" :> "normalize" :> ReqBody '[JSON] DhallRequest :> Get '[JSON] DhallResponse

dhallApi :: Proxy DhallAPI
dhallApi = Proxy

dhallNormalizeGet :: DhallRequest -> AppHandler DhallResponse
dhallNormalizeGet DhallRequest {..} = do
  expr <- liftIO (tryAny (Dhall.inputExpr reqBody)) >>= \case
    Left exn -> throwError err400 { errBody = "Invalid Dhall: " <> show exn}
    Right ok -> pure ok

  json <- dhallToJSON expr & \case
    Left err -> do
      $(logTM) ErrorS $ "Failed to convert Dhall to JSON: " <> show err
      throwError $ err400 { errBody = "Cannot convert Dhall to JSON" }
    Right ok -> pure ok

  pure $ DhallResponse {rspBody = json}

server :: ServerT DhallAPI AppHandler
server = dhallNormalizeGet

nt :: LogItem c => LogEnv -> c -> Namespace -> AppHandler a -> Handler a
nt logEnv c namespace = runKatipContextT logEnv c namespace 

app :: LogItem c => LogEnv -> c -> Namespace -> Application
app logEnv c namespace = serve dhallApi $ hoistServer dhallApi (nt logEnv c namespace) server

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
-- server :: Application
-- server = serve dhallApi dhallNormalizeGet

run :: MonadIO m => LogItem c => LogEnv -> c -> Namespace -> Int -> m ()
run logEnv c namespace port = liftIO $ Network.Wai.Handler.Warp.run port (app logEnv c namespace)
