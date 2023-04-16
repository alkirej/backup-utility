module Main where

import qualified Data.Maybe                  as Mb
import qualified System.Exit                 as Ex

import qualified CommandLine.CommandLine     as Cl
import qualified DirProcessing.DirProcessing as Bkp

main :: IO ()
main = do
    results <- Cl.getBackupDirsFromCommandLine

    if Nothing==results then do
        Ex.exitWith $ Ex.ExitFailure 1
    else do
        let results' = Mb.fromJust results
        let (from,to,verbose,dryRun,force) = results'

        Bkp.backupDir from to verbose dryRun force
        return ()
