module Logging.Logging
    (   logDate,
        logEvent
    )
where

import qualified Data.Time.Clock     as Clk
import qualified Data.Time.Calendar  as Cal
import qualified Data.Time.LocalTime as Locl

subString :: String -> Int -> Int -> String
subString orig start len = subString' orig start len 0 ""

subString' :: String -> Int -> Int -> Int -> String -> String
subString' [] _ _ _ substr = substr
subString' (ch:str) start len current substr =
    if ( current < start ) then
        subString' str start len (current+1) substr
    else if current >= start  &&  current < (start+len) then
        subString' str start len (current+1) (substr ++ [ch])
    else
        substr

timestampString :: IO String
timestampString = do
    tzDateTime <- Locl.getZonedTime
    return $ show tzDateTime

getDate :: IO String
getDate = do
    ts <- timestampString

    let year = subString ts 0 4
        mon  = subString ts 5 2
        day  = subString ts 8 2

    return $ mon ++ "/" ++ day ++ "/" ++ year


logDate :: Bool -> IO ()
logDate dry = do
    dateStr <- getDate
    if dry then
        putStr ("DRY RUN ")
    else
        return ()

    putStr( "Backup date: ")
    putStrLn( dateStr )

getTime :: IO String -- (hour, min)
getTime = do
    ts <- timestampString

    let hr  = subString ts 11 2
        min = subString ts 14 2
        sec = subString ts 17 2

    return $ hr ++ ":" ++ min ++ "." ++ sec

logTime :: IO ()
logTime = do
    time <- getTime
    putStr $ time ++ "  "

logEvent :: String -> IO ()
logEvent event = do
    logTime
    putStrLn event