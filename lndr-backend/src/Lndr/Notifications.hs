{-# LANGUAGE OverloadedStrings #-}

module Lndr.Notifications where

import qualified Data.Map            as M
import           Data.Text           (Text)
import           Lndr.Types
import qualified Network.HTTP.Client as HTTP (applyBasicAuth)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types  as HTTP (hAccept)


-- | Sends a request to the Urban Airship api to send push notifications to
-- specific mobile devices.
sendNotification :: ServerConfig -> Notification -> IO Int
sendNotification config notification = do
    initReq <- HTTP.parseRequest notificationUrl
    let req = HTTP.addRequestHeader HTTP.hAccept acceptContent $
                    HTTP.addRequestHeader "x-api-key" (notificationsApiKey config) $
                    HTTP.setRequestBodyJSON notification $ HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req
    where acceptContent = "application/json"
          notificationUrl = (notificationsApiUrl config)
