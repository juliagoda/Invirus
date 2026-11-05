module Src.Style (changeStyle, changeStyleDB, resetStyle, startAnimation, printActions) where

import Src.Types (ResponseQuery(..), DatabaseQuery(..), DatabaseAction(..))
import System.Console.ANSI
import System.IO
import Control.Concurrent
import Data.Maybe (isJust, fromJust)


malwareFoundStyle :: IO ()
malwareFoundStyle = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Red ]


malwareAddedToDBStyle :: IO ()
malwareAddedToDBStyle = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Green ]


resetStyle :: IO ()
resetStyle = do
    setSGR [ Reset ]


changeStyle :: ResponseQuery -> IO ()
changeStyle Ok = malwareFoundStyle
changeStyle _ = return ()


changeStyleDB :: DatabaseQuery -> DatabaseAction -> IO ()
changeStyleDB Success Find = malwareFoundStyle
changeStyleDB Success Add = malwareAddedToDBStyle
changeStyleDB Fail Add = malwareFoundStyle
changeStyleDB _ _ = return ()


startAnimation :: IO ()
startAnimation = resetStyle >> printAnimation


pause :: IO ()
pause = do
    hFlush stdout
    -- 0.5 second pause
    threadDelay 5000


printAnimation :: IO ()
printAnimation = do
    cursorPos <- getCursorPosition
    let extractedCursorPos = if isJust cursorPos then fromJust cursorPos else (0, 0)
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "Scanning."
    pause
    setCursorPosition ((fst extractedCursorPos) - 1) (snd extractedCursorPos)
    putStrLn "Scanning.."
    pause
    setCursorPosition ((fst extractedCursorPos) - 1) (snd extractedCursorPos)
    putStrLn "Scanning..."
    pause
    setCursorPosition ((fst extractedCursorPos) - 1) (snd extractedCursorPos)


printActions :: IO ()
printActions = do
    putStrLn ""
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "(R)emove file, (P)rint file, (C)ontinue"
    resetStyle