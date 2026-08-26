{-# LANGUAGE OverloadedStrings #-}

module Src.Request (getMalwareInfo) where

import Src.Types (Attempts, Print(..), ResponseQuery(..))
import Src.Style (changeStyle, resetStyle, startAnimation)
import Control.Lens
import Data.Aeson (object, (.=), Value)
import Data.Aeson.Lens (key, nth)
import Network.Wreq
import Control.Monad (when)
import Crypto.Hash (MD5(..), Digest)
import Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromJust)


convertToResponseQuery :: Maybe Value -> ResponseQuery
convertToResponseQuery  (Just "ok") = Ok
convertToResponseQuery (Just "illegal_hash") = IllegalHash
convertToResponseQuery (Just "hash_not_found") = HashNotFound
convertToResponseQuery _ = Undefined


sendRequest :: FilePath -> String -> Attempts -> IO (Either String (Response Data.ByteString.Lazy.Internal.ByteString))
sendRequest filepath hash attempts
    | attempts > 0 = do
        startAnimation
        let opts = defaults & header "referer" .~ ["https://virusvault.vercel.app/bulk-query"]
        let payload = object ["query" Data.Aeson..= ("get_info" :: String), "hash" Data.Aeson..= (hash :: String)]
        result <- postWith opts "https://virusvault.vercel.app/api/malware" payload
        let responseCode = result ^. responseStatus . statusCode
        if (responseCode /= 200) 
            then print (attemptErrorMessage (show responseCode)) >> sendRequest filepath hash (attempts - 1)
            else return $ Right result
    | otherwise = return $ Left $ "All attempts failed for file " ++ filepath ++ " and hash " ++ hash ++ ". Skipping"
    where attemptErrorMessage code = "Error occured during request send: " ++ code ++ ". Trying again." 


getMalwareInfo :: FilePath -> Digest MD5 -> IO ()
getMalwareInfo filepath hash = do
    requestResult <- sendRequest filepath hashString 2
    case requestResult of
        Left message -> print message
        Right result -> do
            let responseQueryStatus = result ^? responseBody . Data.Aeson.Lens.key "query_status"
            let responseData = result ^? responseBody . Data.Aeson.Lens.key "data" . nth 0
            let responseQuery = convertToResponseQuery responseQueryStatus
            changeStyle responseQuery
            printQuery responseQuery filepath
            resetStyle
            when (responseQuery == Ok) $ B8.putStrLn $ encodePretty $ Data.Maybe.fromJust responseData
    where hashString = show hash

