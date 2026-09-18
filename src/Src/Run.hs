module Src.Run (runAPIRequestWithDB, runDBOption) where

import Src.Request (getMalwareInfo)
import Src.Database (addOrAlterDBRow, convertToResponseDB, DatabaseRow(..))
import Src.Actions (chooseAction)
import Src.Types (Print(..), DatabaseQuery(..), DatabaseAction(..))
import Src.Style (changeStyleDB, resetStyle)
import Database.HDBC (SqlValue)
import Database.HDBC.Sqlite3 (Connection)
import Crypto.Hash (MD5(..), Digest)
import Control.Monad (when)


runAPIRequestWithDB :: Connection -> Maybe [SqlValue] -> FilePath -> Digest MD5 -> IO ()
runAPIRequestWithDB dbConnection retrievedMalwareInfo filepath digest = do
    let dbRow = DatabaseRow digest
    let convertedDBResponse = convertToResponseDB retrievedMalwareInfo
    success <- runAPIRequestOption filepath digest
    resetStyle
    changeStyleDB convertedDBResponse Add
    when success $ addOrAlterDBRow retrievedMalwareInfo dbConnection dbRow
    resetStyle
    when success $ chooseAction filepath


runAPIRequestOption :: FilePath -> Digest MD5 -> IO Bool
runAPIRequestOption filepath digest = do
    retrievedMalwareInfo <- getMalwareInfo filepath digest
    return retrievedMalwareInfo


runDBOption ::  Maybe [SqlValue] -> FilePath -> IO ()
runDBOption retrievedMalwareInfo filepath = do
    let convertedDBResponse = convertToResponseDB retrievedMalwareInfo
    resetStyle
    changeStyleDB convertedDBResponse Find
    printQuery convertedDBResponse filepath
    putStrLn $ show retrievedMalwareInfo
    resetStyle
    chooseAction filepath
