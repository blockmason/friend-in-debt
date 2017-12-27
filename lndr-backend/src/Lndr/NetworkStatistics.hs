{-# LANGUAGE ScopedTypeVariables #-}

module Lndr.NetworkStatistics where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class
import           Lndr.Types
import qualified Network.HTTP.Simple as HTTP


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

-- TODO add error handling
queryEtheruemPrice :: IO EthereumPrice
queryEtheruemPrice = do
    req <- HTTP.parseRequest "https://api.coinbase.com/v2/exchange-rates?currency=ETH"
    HTTP.getResponseBody <$> HTTP.httpJSON req