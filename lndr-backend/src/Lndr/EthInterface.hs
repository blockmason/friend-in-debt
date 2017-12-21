{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}

module Lndr.EthInterface where

import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as BS16
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Default
import           Data.Either (rights)
import qualified Data.HashMap.Strict as H (lookup)
import           Data.List.Safe ((!!))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3 hiding (convert)
import qualified Network.Ethereum.Web3.Eth as Eth
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
import qualified Network.HTTP.Client as HTTP (applyBasicAuth)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP (hAccept)
import           Prelude hiding (lookup, (!!))
import           System.FilePath

instance Configured Address where
    convert (String x) = Just $ textToAddress x
    convert _ = Nothing

loadConfig :: IO ServerConfig
loadConfig = do
    config <- getMap =<< load [Required $ "lndr-backend" </> "data" </> "lndr-server.config"]
    let loadEntry x = fromMaybe (error $ T.unpack x) $ convert =<< H.lookup x config
    return $ ServerConfig (loadEntry "lndrUcacAddr")
                          (loadEntry "creditProtocolAddress")
                          (loadEntry "issueCreditEvent")
                          (loadEntry "scanStartBlock")
                          (loadEntry "dbUser")
                          (loadEntry "dbUserPassword")
                          (loadEntry "dbName")
                          (loadEntry "executionAddress")
                          (loadEntry "gasPrice")
                          (loadEntry "maxGas")
                          (loadEntry "urbanAirshipKey")
                          (loadEntry "urbanAirshipSecret")


-- Create functions to call CreditProtocol contract. Currently, only `issueCredit` is used.
[abiFrom|data/CreditProtocol.abi|]


sendNotification :: ServerConfig -> Notification -> IO Int
sendNotification config notification = do
    initReq <- HTTP.parseRequest urbanAirshipUrl
    let req = HTTP.addRequestHeader HTTP.hAccept acceptContent $
                    HTTP.applyBasicAuth (urbanAirshipKey config) (urbanAirshipSecret config) $
                    HTTP.setRequestBodyJSON notification $ HTTP.setRequestMethod "POST" initReq
    HTTP.getResponseStatusCode <$> HTTP.httpNoBody req
    where acceptContent = "application/vnd.urbanairship+json; version=3;"
          urbanAirshipUrl = "https://go.urbanairship.com/api/push"


safelowUpdate :: ServerConfig -> TVar ServerConfig -> IO ServerConfig
safelowUpdate config configTVar = do
    req <- HTTP.parseRequest "https://ethgasstation.info/json/ethgasAPI.json"
    gasStationResponseE <- try (HTTP.getResponseBody <$> HTTP.httpJSON req)
    case gasStationResponseE of
        Right gasStationResponse -> do
            let lastestSafeLow = ceiling $ margin * safeLowScaling * safeLow gasStationResponse
                updatedConfg = config { gasPrice = lastestSafeLow }
            liftIO . atomically . modifyTVar configTVar $ const updatedConfg
            return updatedConfg
        Left (_ :: HTTP.HttpException) -> return config
    where
        safeLowScaling = 100000000 -- eth gas station returns prices in DeciGigaWei
        margin = 1.3 -- multiplier for  additional assurance that tx will make it into blockchain

finalizeTransaction :: ServerConfig -> Text -> Text -> CreditRecord
                    -> IO (Either Web3Error TxHash)
finalizeTransaction config sig1 sig2 (CreditRecord creditor debtor amount memo _ _ _ _) = do
      let (sig1r, sig1s, sig1v) = decomposeSig sig1
          (sig2r, sig2s, sig2v) = decomposeSig sig2
          encodedMemo :: BytesN 32
          encodedMemo = BytesN . BA.convert . T.encodeUtf8 $ memo
      runWeb3 $ issueCredit callVal
                            (lndrUcacAddr config)
                            creditor debtor amount
                            [ sig1r, sig1s, sig1v ]
                            [ sig2r, sig2s, sig2v ]
                            encodedMemo
    where callVal = def { callFrom = Just $ executionAddress config
                        , callTo = creditProtocolAddress config
                        , callGasPrice = Just . Quantity $ gasPrice config
                        , callValue = Just . Quantity $ 0
                        , callGas = Just . Quantity $ maxGas config
                        }

lndrLogs :: Provider a => ServerConfig -> Maybe Address -> Maybe Address
         -> Web3 a [IssueCreditLog]
lndrLogs config p1M p2M = rights . fmap interpretUcacLog <$>
    Eth.getLogs (Filter (Just $ creditProtocolAddress config)
                        (Just [ Just (issueCreditEvent config)
                              , Just (addressToBytes32 $ lndrUcacAddr config)
                              , addressToBytes32 <$> p1M
                              , addressToBytes32 <$> p2M ])
                        (Just . integerToHex' $ scanStartBlock config)
                        Nothing)


interpretUcacLog :: Change -> Either SomeException IssueCreditLog
interpretUcacLog change = do
    ucacAddr <- bytes32ToAddress <=< (!! 1) $ changeTopics change
    creditorAddr <- bytes32ToAddress <=< (!! 2) $ changeTopics change
    debtorAddr <- bytes32ToAddress <=< (!! 3) $ changeTopics change
    let amount = hexToInteger . takeNthByte32 0 $ changeData change
        nonce = hexToInteger . takeNthByte32 1 $ changeData change
        memo = T.decodeUtf8 . fst . BS16.decode . T.encodeUtf8 . takeNthByte32 2 $ changeData change
    pure $ IssueCreditLog ucacAddr
                          creditorAddr
                          debtorAddr
                          amount
                          nonce
                          memo
