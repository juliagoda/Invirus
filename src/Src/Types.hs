module Src.Types (Attempts, Print(..), ResponseQuery(..)) where


type Attempts = Int

class Print a where
    printQuery :: a -> FilePath -> IO ()

data ResponseQuery = Ok | IllegalHash | HashNotFound | Undefined deriving (Eq, Show)

instance Print ResponseQuery where
    printQuery Ok filepath = print $ "Malware has been found for " ++ filepath
    printQuery IllegalHash filepath = print $ "Incorrect hash has been used to find samples for " ++ filepath
    printQuery HashNotFound filepath = print $ "No malware samples found for the provided hash from file " ++ filepath
    printQuery Undefined filepath = print $ "Not known error when sending request for " ++ filepath