module Src.Run (runAPIRequestWithDB, runDBOption) where

import Src.Request (getMalwareInfo)
import Src.Database (addOrAlterDBRow, convertToResponseDB)
import Src.Actions (chooseAction)
import Src.Types (DigestValue, DigestType, Print(..), DatabaseAction(..))
import Src.Style (changeStyleDB, resetStyle)
import Database.HDBC (SqlValue)
import Database.HDBC.Sqlite3 (Connection)
import Control.Monad (when)


runAPIRequestWithDB :: Connection -> Maybe [SqlValue] -> FilePath -> DigestValue -> DigestType -> IO ()
runAPIRequestWithDB dbConnection retrievedMalwareInfo filepath digest hash = do
    let convertedDBResponse = convertToResponseDB retrievedMalwareInfo
    success <- runAPIRequestOption filepath digest
    resetStyle
    changeStyleDB convertedDBResponse Add
    when success $ addOrAlterDBRow hash retrievedMalwareInfo dbConnection filepath digest
    resetStyle
    when success $ chooseAction filepath


runAPIRequestOption :: FilePath -> DigestValue -> IO Bool
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
