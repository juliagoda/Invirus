module Src.Style (changeStyle, resetStyle, startAnimation, printActions) where

import Src.Types (ResponseQuery(..))
import System.Console.ANSI
import System.IO
import Control.Concurrent
import Data.Maybe (isJust, fromJust)


malwareFoundStyle :: IO ()
malwareFoundStyle = do
    setSGR [ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Vivid Red ]


resetStyle :: IO ()
resetStyle = do
    setSGR [ Reset ]


changeStyle :: ResponseQuery -> IO ()
changeStyle Ok = malwareFoundStyle
changeStyle _ = return ()


startAnimation :: IO ()
startAnimation = resetStyle >> printAnimation


pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 1000000


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