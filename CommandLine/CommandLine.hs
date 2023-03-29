module CommandLine.CommandLine
    (   getBackupDirsFromCommandLine,
        handleErrors,
    )
where

import qualified Control.Exception     as Exc
import qualified Data.Either           as Ei
import qualified Data.Maybe            as Mb
import qualified System.Console.GetOpt as Opt
import qualified System.Directory      as Dir
import qualified System.Directory.Internal.Prelude as Dirp
import qualified System.Environment    as Env

-- ********************************************************************
-- GENERAL SETUP
-- ********************************************************************
-- --------------------------------------------------------------------
-- Flags allowed for the backup utility.
-- --------------------------------------------------------------------
data Flag = FromDir FilePath |
            ToDir   FilePath |
            DryRun           |
            Force            |
            Verbose
  deriving (Show, Eq)

-- --------------------------------------------------------------------
-- getOpts options parameter setup
-- --------------------------------------------------------------------
options :: [Opt.OptDescr Flag]
options =
    [ Opt.Option ['f'] ["from"]    (Opt.ReqArg FromDir "DIR")
                                   "location of original data",
      Opt.Option ['t'] ["to"]      (Opt.ReqArg ToDir "DIR")
                                   "location to store backup data",
      Opt.Option ['v'] ["verbose"] (Opt.NoArg Verbose)
                                   "chatty output on stderr",
      Opt.Option []    ["FORCE"]   (Opt.NoArg Force)
                                   "force backup despite possible trouble",
      Opt.Option ['d'] ["dry-run"] (Opt.NoArg DryRun)
                                   "dry run of backup"
    ]

-- --------------------------------------------------------------------
-- Show program usage.
-- --------------------------------------------------------------------
showUsage :: IO ()
showUsage = do
    putStrLn $ Opt.usageInfo "USAGE: backup-util -f <DIR> -t <DIR> [-v]" options

-- ********************************************************************
-- ERROR/WARNING HANDLING
-- ********************************************************************
-- --------------------------------------------------------------------
-- Errors have been found.  Report them th the user.
-- --------------------------------------------------------------------
handleErrors :: [String] -> IO Bool
handleErrors errors = do
    putStrLn ""
    putStrLn "Error encountered:"
    handleErrors' errors
    putStrLn ""
    showUsage
    return False

-- recursive helper in case multiple errors found.
handleErrors' :: [String] -> IO ()
handleErrors' []         = return ()
handleErrors' (err:errs) = do
    putStr $ "    " ++ err
    handleErrors' errs

-- --------------------------------------------------------------------
-- An error was encountered during directory interaction.
-- --------------------------------------------------------------------
directoryError :: String -> IOError -> IO Bool
directoryError from e
    | Dirp.isDoesNotExistError e = do
            handleErrors [ "Cannot find directory: "++from++".\n" ]
            return False
    | otherwise = do
            handleErrors [ (show e)++"\n" ]
            return False

-- --------------------------------------------------------------------
-- Display warning message to user, but continue on.
-- --------------------------------------------------------------------
handleWarning :: String -> IO Bool
handleWarning message = do
    putStrLn ""
    putStrLn $ "Warning: " ++ message ++ "\n"
    return True

-- ********************************************************************
-- COMMAND LINE OPTION VERIFICATION
-- ********************************************************************
-- --------------------------------------------------------------------
-- Check the command-line parameters for correctness.
-- --------------------------------------------------------------------
verifyOptions :: ([Flag],[String]) -> IO Bool
verifyOptions ([],_) = do
    putStrLn ""
    showUsage
    return False
verifyOptions (flags,[]) = do
    return True
verifyOptions (flags,others) = do
    let extraMsgs  = map ("Unnecessary command line parameter: "++)
                         [ o | o <- others ]
        extraMsgs' = map (++"\n") extraMsgs
    handleErrors extraMsgs'
    return False

-- --------------------------------------------------------------------
-- Inspect from and to directories for correctness.
-- --------------------------------------------------------------------
verifyDirectories :: [Flag] -> IO Bool
verifyDirectories flags = do
    (from,to,errs) <- getDirs flags

    if not $ null errs then
        handleErrors errs
    else
        verifyDirectories' from to

-- helper for verifyDirectories
verifyDirectories' :: FilePath -> FilePath -> IO Bool
verifyDirectories' from to = do
    exists <- Exc.catch (testDirExists from) (directoryError from)
    if not exists then
        return exists
    else
        Exc.catch (testDirExists to) (createDir to)


-- ********************************************************************
-- DISK INTERACTIONS
-- ********************************************************************
-- --------------------------------------------------------------------
-- Does the supplied directory exist?
-- --------------------------------------------------------------------
testDirExists :: FilePath -> IO Bool
testDirExists from = do
    Dir.listDirectory from
    return True

-- --------------------------------------------------------------------
-- Attempt to create the supplied directory.
-- --------------------------------------------------------------------
createDir :: FilePath -> IOError -> IO Bool
createDir newDir e
    | Dirp.isDoesNotExistError e = do
            handleWarning $ "Directory " ++ newDir ++ " does not exist. "
                                ++ "Attempting to create."
            Dir.createDirectoryIfMissing True newDir
            return True
    | otherwise = do
            handleErrors [(show e)++"\n"]
            return False

-- --------------------------------------------------------------------
-- Given the command line arguments, retreive the from and to dirs.
--      Make sure to give a useful error message.
-- --------------------------------------------------------------------
getDirs :: [Flag] -> IO ( FilePath, FilePath, [String] )
getDirs flags = getDirs' flags Nothing Nothing []

getDirs' :: [Flag] -> Maybe FilePath -> Maybe FilePath -> [String]
                   -> IO (FilePath, FilePath, [String])
-- to & from locations have been found.
getDirs' _ (Just from) (Just to) errs = return (from, to, errs)

-- we are out of flags without finding what we need.  Report error.
getDirs' [] a b errs   = do
        return ( "","", ("Invalid FROM and/or TO locations\n":errs) )

-- we have found a from directory.
getDirs' (FromDir name:fs) from to errs =
    if Mb.isJust from then
        getDirs' fs from to ("Multiple FROM locations found\n":errs)
    else do
        fullPath <- Dir.makeAbsolute name
        getDirs' fs (Mb.Just fullPath) to errs

-- we have found a to directory.
getDirs' (ToDir name:fs) from to errs =
    if Mb.isJust to then
        getDirs' fs from to ("Multiple TO locations found\n":errs)
    else do
        fullPath <- Dir.makeAbsolute name
        getDirs' fs from (Mb.Just fullPath) errs

-- something unexpected happened. Recurse to an error condition.
getDirs' (f:fs) from to errs = getDirs' fs from to errs


-- ********************************************************************
-- GET WHAT WE NEED FROM THE COMMAND LINE
-- ********************************************************************
-- --------------------------------------------------------------------
-- Give verbose output?
-- --------------------------------------------------------------------
beVerbose :: [Flag] -> Bool
beVerbose flags = any (==Verbose) flags

-- --------------------------------------------------------------------
-- Give verbose output?
-- --------------------------------------------------------------------
doDryRun :: [Flag] -> Bool
doDryRun flags = any (==DryRun) flags

-- --------------------------------------------------------------------
-- Give verbose output?
-- --------------------------------------------------------------------
forceBackup :: [Flag] -> Bool
forceBackup flags = any (==Force) flags

-- --------------------------------------------------------------------
-- Use getOpts to convert the connand line into easy to handle info.
-- --------------------------------------------------------------------
processCommandLine :: [String] -> IO ( Either [String] ([Flag],[String]) )
processCommandLine commandLine =
    case Opt.getOpt Opt.Permute options commandLine of
        (o,n,[])     -> return $ Ei.Right (o,n)
        (_,_,errors) -> return $ Ei.Left errors

-- --------------------------------------------------------------------
-- Read the command line and look for from & to directories.
--      If an error is encountered, return Nothing.
--      Otherwise, return the triple (from dir, to dir, verbose output?)
-- --------------------------------------------------------------------
getBackupDirsFromCommandLine :: IO ( Maybe (FilePath,FilePath,Bool,Bool,Bool) )
getBackupDirsFromCommandLine = do
    -- read command line and check for obvious errors
    args  <- Env.getArgs
    cl    <- processCommandLine args
    good  <- if Ei.isLeft cl then
                handleErrors $ Ei.fromLeft [] cl
             else
                verifyOptions $ Ei.fromRight ([],[]) cl

    -- get the flags set by the command line
    let flags = if good then
                    fst (Ei.fromRight ([],[]) cl)
                 else
                    []

    -- no flags = error
    if null flags then
        return Nothing
    -- take flags and make sure the directories are available.
    else do
        verified <- verifyDirectories flags
        if verified then do
            let verbose = beVerbose   flags
                dryRun  = doDryRun    flags
                force   = forceBackup flags
            (fromDir,toDir,errors) <- getDirs flags
            if null errors then
                return $ Just ( fromDir, toDir, verbose, dryRun, force )
            else do
                -- error found trying to determine the dirs.
                handleErrors errors
                return Nothing

        -- couldn't verify the directories
        else return Nothing