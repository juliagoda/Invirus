module Src.Config (getConfig, getOptDigest) where

import Data.Ini.Config
import Data.Text (pack)
import qualified Data.Text.IO as T
import Data.Either (fromRight)
import qualified Data.ByteString.Char8 as BS
import Paths_Invirus (getDataFileName)


data OptionsConfig = OptionsConfig
  { optDigest :: String }
    deriving (Eq, Show)

data Config = Config
  { cfOptions :: OptionsConfig }
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config { cfOptions = OptionsConfig { optDigest = "md5" }}


configParser :: IniParser Config
configParser = do
  optCfg <- section (pack "OPTIONS") $ do
    digestTxt <- fieldOf (pack "digest") string
    return OptionsConfig { optDigest = digestTxt }
  return Config { cfOptions = optCfg }


getConfig :: FilePath -> IO Config
getConfig filepath = do
    configFile <- getDataFileName filepath
    content <- T.readFile configFile
    let parsedFile = parseIniFile content configParser
    return $ fromRight defaultConfig parsedFile


getOptDigest :: IO String
getOptDigest = optDigest . cfOptions <$> getConfig "config/config.ini"
