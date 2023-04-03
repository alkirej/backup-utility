module Main where

import qualified Data.Maybe                  as Mb
import qualified System.Exit                 as Ex

import qualified CommandLine.CommandLine     as Cl
import qualified DirProcessing.DirProcessing as Bkp

firstFive :: String -> String
firstFive str = firstFive' str 0 ""

firstFive' :: String -> Int -> String -> String
firstFive' [] _ out = out
firstFive' _  5 out = out
firstFive' (c:cs) soFar out = firstFive' cs (soFar+1) (out++[c])

nfsDir :: FilePath -> Bool
nfsDir dir = firstFive dir == "/nfs/"

main :: IO ()
main = do
    results <- Cl.getBackupDirsFromCommandLine

    if Nothing==results then do
        Ex.exitWith $ Ex.ExitFailure 1
    else do
        let results' = Mb.fromJust results
        let (from,to,verbose,dryRun,force) = results'

        -- protect nfs dirs
        if (nfsDir to) && (not force) then do
            Cl.handleErrors [ "CANNOT BACKUP TO NFS DIRECTORY! "
                                ++ "This could do BAD, VERY BAD things!\n"
                            ]
            Ex.exitWith $ Ex.ExitFailure 2
        else do
            Bkp.backupDir from to verbose dryRun force
            return ()
