module Lib
    ( scanFiles
    , module Src.Scan
    , module Src.Request
    ) where

import Src.Scan (collectFiles)
import Src.Request (getMalwareInfo)
import System.FilePath (isAbsolute)
import Control.Exception (throwIO, Exception)
import Data.Typeable
import Control.Monad (when, forM_)
import System.Directory (doesDirectoryExist)
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
    collectedFiles <- Streamly.Data.Stream.fold Streamly.Data.Fold.toList $ collectFiles systemPath
    forM_ collectedFiles $ \(digest, path) -> do
        getMalwareInfo path digest
