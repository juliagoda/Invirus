module Lib
    ( scanFiles
    , module Src.Scan
    , module Src.Database
    , module Src.Run
    ) where

import Src.Scan (collectFiles)
import Src.Database (malwareInDB, createDB, addOrAlterDBRow, convertToResponseDB)
import Src.Types (DatabaseQuery(..))
import Src.Run (runDBOption, runAPIRequestWithDB)
import Src.Config (getOptDigest)
import System.FilePath (isAbsolute)
import Control.Exception (throwIO, Exception)
import Data.Typeable
import Control.Monad (when, forM_)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import Streamly.Data.Fold (toList)
import Streamly.Data.Stream (fold)

data PathError = PathError String deriving (Show, Typeable)

instance Exception PathError


scanFiles :: IO ()
scanFiles = do
    putStrLn "Choose absolute directory path for scanning"
    systemPath <- getLine
    existsSystemPath <- doesDirectoryExist systemPath
    when (not $ existsSystemPath && isAbsolute systemPath) $ throwIO (PathError "The given directory path doesn't exist")
    getCurrentDirectory  >>= putStrLn
    optDigest <- getOptDigest 
    collectedFiles <- Streamly.Data.Stream.fold Streamly.Data.Fold.toList $ collectFiles optDigest systemPath
    dbConnection <- createDB
    forM_ collectedFiles $ \(digest, path) -> do
        retrievedMalwareInfo <- malwareInDB optDigest dbConnection digest
        let convertedDBResponse = convertToResponseDB retrievedMalwareInfo
        if convertedDBResponse == Success then runDBOption retrievedMalwareInfo path else runAPIRequestWithDB dbConnection retrievedMalwareInfo path digest optDigest