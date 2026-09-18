module Src.Types (Attempts, Print(..), ResponseQuery(..), DatabaseQuery(..), DatabaseAction(..)) where


type Attempts = Int

class Print a where
    printQuery :: a -> FilePath -> IO ()

data ResponseQuery = Ok | IllegalHash | HashNotFound | Undefined deriving (Eq, Show)
data DatabaseQuery = Success | Fail deriving (Eq, Show)
data DatabaseAction = Add | Find deriving (Show)

instance Print ResponseQuery where
    printQuery Ok filepath = print $ "Malware has been found for " ++ filepath
    printQuery IllegalHash filepath = print $ "Incorrect hash has been used to find samples for " ++ filepath
    printQuery HashNotFound filepath = print $ "No malware samples found for the provided hash from file " ++ filepath
    printQuery Undefined filepath = print $ "Not known error when sending request for " ++ filepath

instance Print DatabaseQuery where
    printQuery Success filepath = print $ "Malware has been found for " ++ filepath ++ " in database"
    printQuery Fail filepath = print $ "Error during request send to database for " ++ filepath