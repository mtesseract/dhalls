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
import Servant.Server (Handler, serve)

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

dhallNormalizeGet :: DhallRequest -> Handler DhallResponse
dhallNormalizeGet DhallRequest {..} = do
  expr <- liftIO $ Dhall.inputExpr reqBody
  case dhallToJSON expr of
    Left err -> undefined
    Right res -> pure DhallResponse {rspBody = res}

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
server :: Application
server = serve dhallApi dhallNormalizeGet

run :: MonadIO m => Int -> m ()
run port = liftIO $ Network.Wai.Handler.Warp.run port server
