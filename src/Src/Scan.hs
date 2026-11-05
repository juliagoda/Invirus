module Src.Scan where

import Streamly.Data.Stream (Stream, mapM, filterM, splitOn)
import Streamly.Data.Fold (toList)
import qualified Streamly.System.Process as Process
import Data.Function ((&))
import Crypto.Hash (MD5(..), SHA1(..), SHA256(..),
                    hashInit, hashUpdate, hashFinalize,
                    Digest, HashAlgorithm)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as BS
import System.IO (withBinaryFile, IOMode(ReadMode), hIsEOF)
import Data.Char (chr)
import Src.Types (DigestType, DigestValue)


bsToStr :: BS.ByteString -> String
bsToStr = map (chr . fromEnum) . BS.unpack


-- The _alg parameter is only there to pin the type — it's never used at runtime. 
-- hashInit produces the correct Digest a purely from the type.
hashFile :: HashAlgorithm a => a -> FilePath -> IO (Digest a)
hashFile _alg filePath = withBinaryFile filePath ReadMode $ \h -> do
    let loop ctx = do
            isEof <- hIsEOF h
            if isEof
                then return (hashFinalize ctx)   -- wrap in return
                else do
                    chunk <- BS.hGetSome h 4096
                    loop (hashUpdate ctx chunk)
    loop hashInit


generateHashFile :: HashAlgorithm a => a -> FilePath -> IO BS.ByteString
generateHashFile digest filePath = hashFile digest filePath >>= return . convertToBase Base16

generateHashMap :: DigestType -> FilePath -> IO (DigestValue, FilePath)
generateHashMap alg filePath = do
    bs <- case alg of
        "sha1"   -> generateHashFile SHA1 filePath
        "sha256" -> generateHashFile SHA256 filePath
        _        -> generateHashFile MD5 filePath
    return (bsToStr bs, filePath)


generateHashesForFile :: FilePath -> IO (DigestValue, DigestValue, DigestValue)
generateHashesForFile filePath = do
    sha1 <- generateHashFile SHA1 filePath
    sha256 <- generateHashFile SHA256 filePath
    md5 <- generateHashFile MD5 filePath
    return (bsToStr md5, bsToStr sha1, bsToStr sha256)


collectFiles :: DigestType -> FilePath -> Stream IO (DigestValue, FilePath)
collectFiles alg path = do
    Process.toChars "find" [path, "-type", "f", "!", "-name", ".*"]
        & splitOn (== '\n') toList
        & Streamly.Data.Stream.filterM (\x -> return $ (not . null) x)
        & Streamly.Data.Stream.mapM (generateHashMap alg)