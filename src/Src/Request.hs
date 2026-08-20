{-# LANGUAGE OverloadedStrings #-}

module Src.Request (getMalwareInfo) where

import Control.Lens
import Data.Aeson (object, (.=), Value)
import Data.Aeson.Lens (key, nth)
import Network.Wreq
import System.Time.Extra (sleep)
import Control.Monad (when)
import Crypto.Hash (MD5(..), Digest)
import Data.ByteString.Lazy.Internal

type Attempts = Int

class Print a where
    printQuery :: a -> FilePath -> IO ()

data ResponseQuery = Ok | IllegalHash | HashNotFound | Undefined deriving (Eq, Show)

instance Print ResponseQuery where
    printQuery Ok filepath = print $ "Malware has been found for " ++ filepath
    printQuery IllegalHash filepath = print $ "Incorrect hash has been used to find samples for " ++ filepath
    printQuery HashNotFound filepath = print $ "No malware samples found for the provided hash " ++ filepath
    printQuery Undefined filepath = print $ "Not known error when sending request for " ++ filepath


convertToResponseQuery :: Maybe Value -> ResponseQuery
convertToResponseQuery  (Just "ok") = Ok
convertToResponseQuery (Just "illegal_hash") = IllegalHash
convertToResponseQuery (Just "hash_not_found") = HashNotFound
convertToResponseQuery _ = Undefined


sendRequest :: FilePath -> String -> Attempts -> IO (Either String (Response Data.ByteString.Lazy.Internal.ByteString))
sendRequest filepath hash attempts
    | attempts > 0 = do
        sleep 5
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
            printQuery responseQuery filepath
            when (responseQuery == Ok) $ print responseData
    where hashString = show hash
