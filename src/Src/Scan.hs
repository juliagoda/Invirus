module Src.Scan where

import Streamly.Data.Stream (Stream, fold, mapM, filterM, splitOn)
import Streamly.Data.Fold (Fold, drain, toList)
import qualified Streamly.System.Process as Process
import Data.Function ((&))
import Crypto.Hash (MD5(..), hashInit, hashUpdate, hashFinalize, Digest, hashInitWith)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import System.IO (withBinaryFile, IOMode(ReadMode), hIsEOF)


generateHashMap :: FilePath -> IO (Digest MD5, FilePath)
generateHashMap filePath = do
    print filePath
    digest <- withBinaryFile filePath ReadMode $ \h -> do
        let loop ctx = do
                isEof <- hIsEOF h
                if isEof
                    then return $ hashFinalize ctx
                    else do
                        chunk <- BS.hGetSome h 4096
                        let ctx' = hashUpdate ctx chunk
                        loop ctx'

        let initialCtx = hashInitWith MD5
        loop initialCtx

    return (digest, filePath)


collectFiles :: FilePath -> Stream IO (Digest MD5, FilePath)
collectFiles path = do
    Process.toChars "find" [path, "-type", "f", "!", "-name", ".*"]
        & splitOn (== '\n') toList
        & Streamly.Data.Stream.filterM (\x -> return $ (not . null) x)
        & Streamly.Data.Stream.mapM generateHashMap