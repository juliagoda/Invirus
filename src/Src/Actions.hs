module Src.Actions (chooseAction) where

import Src.Style (printActions)
import System.Directory (doesFileExist, removeFile)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import System.Console.Haskeline (runInputT, defaultSettings, getInputChar)

data Action = Remove | Print | Continue deriving (Show)

keyMap :: Map.Map Char Action
keyMap = Map.fromList [ ('r', Remove), ('p', Print), ('c', Continue) ]


removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filepath = do
    exists <- doesFileExist filepath
    if exists then removeFile filepath >> print "File has been removed" else print "File couldn't be removed"
 

printFileIfExists :: FilePath -> IO ()
printFileIfExists filepath = do
    exists <- doesFileExist filepath
    if exists then BL.readFile filepath >>= BL.putStr else print "File couldn't be printed"


runAction :: Action -> FilePath -> IO ()
runAction Remove = removeFileIfExists
runAction Print = printFileIfExists
runAction Continue = print . ("Skipping action for "++) 


chooseAction :: FilePath -> IO ()
chooseAction filepath = do
    printActions
    pressedKey <- runInputT defaultSettings (getInputChar "Press a key: ")
    case pressedKey of
        Nothing -> print "Incorrect key pressed. Try again" >> chooseAction filepath
        Just a -> case Map.lookup a keyMap of
            Just Continue -> runAction Continue filepath
            Just action -> runAction action filepath >> chooseAction filepath
            Nothing -> print "Incorrect key pressed. Try again" >> chooseAction filepath
