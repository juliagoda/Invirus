module Src.Database (convertToResponseDB, createDB, malwareInDB, addOrAlterDBRow, disconnectDB, DatabaseRow(..)) where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Src.Types (Print(..), DatabaseQuery(..))
import Crypto.Hash (MD5(..), Digest)

data DatabaseRow = DatabaseRow { md5 :: Digest MD5 } deriving (Show)


convertToResponseDB :: Maybe [SqlValue] -> DatabaseQuery
convertToResponseDB  (Just _) = Success
convertToResponseDB Nothing = Fail


createDB :: IO Connection
createDB = do
    connection <- connectSqlite3 "malware.db"
    handleSqlError $ run connection "CREATE TABLE IF NOT EXISTS malware (md5 TEXT NOT NULL, date DATETIME NOT NULL)" []
    return connection


prepareCorrectQuery :: DatabaseRow -> Maybe [SqlValue] -> String
prepareCorrectQuery databaseRow Nothing  = "INSERT INTO malware VALUES ('" ++ (show $ md5 databaseRow) ++ "', datetime('now', 'localtime'));"
prepareCorrectQuery databaseRow _  = "UPDATE malware SET date = datetime('now', 'localtime') WHERE md5 = '" ++ (show $ md5 databaseRow) ++ "';"


malwareInDB :: Connection -> Digest MD5 -> IO (Maybe [SqlValue])
malwareInDB connection digest = do
    select <- prepare connection $ "SELECT * FROM malware WHERE md5 = '" ++ (show digest) ++ "';"
    execute select []
    fetchRow select


addOrAlterDBRow :: Maybe [SqlValue] -> Connection -> DatabaseRow -> IO ()
addOrAlterDBRow malware connection databaseRow = do
    handleSqlError $ run connection (prepareCorrectQuery databaseRow malware) []
    putStrLn $ "Malware with digest " ++ (show $ md5 databaseRow) ++ " has been added to database"
    commit connection


disconnectDB :: Connection -> IO ()
disconnectDB connection = disconnect connection
