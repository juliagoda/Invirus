module Src.Database (convertToResponseDB, createDB, malwareInDB, addOrAlterDBRow, disconnectDB) where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Src.Scan (generateHashesForFile)
import Src.Types (DatabaseQuery(..), DigestValue)


convertToResponseDB :: Maybe [SqlValue] -> DatabaseQuery
convertToResponseDB  (Just _) = Success
convertToResponseDB Nothing = Fail


createDB :: IO Connection
createDB = do
    connection <- connectSqlite3 "malware.db"
    _ <- handleSqlError $ run connection "CREATE TABLE IF NOT EXISTS malware (md5 TEXT NOT NULL, sha1 TEXT NOT NULL, sha256 TEXT NOT NULL, date DATETIME NOT NULL)" []
    return connection


prepareHashQuery :: String -> Maybe [SqlValue] -> FilePath -> DigestValue -> IO String 
prepareHashQuery hash sqlValue filepath digest = do
    case sqlValue of
        Nothing -> do
            hashes <- generateHashesForFile filepath
            let (md5, sha1, sha256) = hashes
            return $ md5 ++ "', '" ++ sha1 ++ "', '" ++ sha256
        _ -> return $ hash ++ " = '" ++ digest


prepareCorrectQuery :: String -> Maybe [SqlValue] -> String
prepareCorrectQuery hashQuery Nothing  = "INSERT INTO malware VALUES ('" ++ hashQuery ++ "', datetime('now', 'localtime'));"
prepareCorrectQuery hashQuery _  = "UPDATE malware SET date = datetime('now', 'localtime') WHERE " ++ hashQuery ++ "';"


malwareInDB :: String -> Connection -> DigestValue -> IO (Maybe [SqlValue])
malwareInDB hash connection digest = do
    putStrLn $ hash ++ " = '" ++ digest ++ "'"
    select <- prepare connection $ "SELECT * FROM malware WHERE " ++ hash ++ " = '" ++ digest ++ "';"
    execute select []
    fetchRow select


addOrAlterDBRow :: String -> Maybe [SqlValue] -> Connection -> FilePath -> DigestValue -> IO ()
addOrAlterDBRow hash malware connection filepath digest = do
    preparedHashQueryPart <- prepareHashQuery hash malware filepath digest
    handleSqlError $ run connection (prepareCorrectQuery preparedHashQueryPart malware) []
    let action = if malware == Nothing then "added to" else "updated in"
    putStrLn $ "Malware with digest/s " ++ preparedHashQueryPart ++ " has been " ++ action ++ " database"
    commit connection


disconnectDB :: Connection -> IO ()
disconnectDB connection = disconnect connection
