module DirProcessing.DirProcessing
    ( backupDir )
where

import qualified Data.List               as Lst
import qualified System.Directory        as Dir

import qualified CommandLine.CommandLine as Cl
import qualified Logging.Logging         as Log

-- --------------------------------------------------------------------
-- Given a directory name and file name, combine the two into a full
--      path.
-- --------------------------------------------------------------------
makePath :: FilePath -> FilePath -> FilePath
makePath dirName fileName = dirName ++ "/" ++ fileName

-- --------------------------------------------------------------------
-- Remove the files in the supplied list.
-- --------------------------------------------------------------------
removeFiles :: FilePath -> [FilePath] -> Bool -> IO ()
removeFiles _ [] _ = return ()
removeFiles dir (f:fs) dryRun = do
    let path = makePath dir f
    isDir <- Dir.doesDirectoryExist path

    if isDir then do
        Log.logEvent $ "REMOVE DIRECTORY: " ++ f
        if dryRun then
            return ()
        else
            Dir.removeDirectoryRecursive path
    else do
        Log.logEvent $ "Remove file: " ++ f
        if dryRun then
            return ()
        else
            Dir.removeFile path

    removeFiles dir fs dryRun

-- --------------------------------------------------------------------
-- Copy the files in the supplied list.
-- --------------------------------------------------------------------
addFiles :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
addFiles _ _ [] _ _ = return ()
addFiles fromDir toDir (f:fs) verbose dryRun = do
    let from = makePath fromDir f
        to   = makePath toDir   f

    isDir <- Dir.doesDirectoryExist from

    if isDir then do
        -- Copy whole dir
        Log.logEvent $ "ADD NEW DIRECTORY " ++ f
        if dryRun then
            return ()
        else
            Dir.createDirectoryIfMissing True to
        backupDir' from to verbose dryRun
        if verbose then
            Log.logEvent $ "ADD NEW DIRECTORY COMPLETE FOR " ++ f
        else
            return ()
    else do
        -- copy 1 file
        Log.logEvent $ "Add new file: " ++ f
        if dryRun then
            return ()
        else
            Dir.copyFileWithMetadata from to

    addFiles fromDir toDir fs verbose dryRun

-- --------------------------------------------------------------------
-- Check the modification times and sizes of the file.  If the file
--      sizes differ, or if the original was created after the backup,
--      the replace the backup with the new file.
-- --------------------------------------------------------------------
replaceFiles :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
replaceFiles _ _ [] _ _ = return ()
replaceFiles fromDir toDir (f:fs) verbose dryRun= do
    let oldFile = makePath fromDir f
        newFile = makePath toDir   f

    -- do subdirs recursively
    isDir <- Dir.doesDirectoryExist oldFile

    if isDir then
        backupDir' oldFile newFile verbose dryRun

    else do
        oldSize <- Dir.getFileSize oldFile
        newSize <- Dir.getFileSize newFile
        oldTime <- Dir.getModificationTime oldFile
        newTime <- Dir.getModificationTime newFile

        if oldSize == newSize  &&  oldTime >= newTime then
            if verbose then
                Log.logEvent $ "File " ++ f ++ " left unchanged."
            else
                -- file has not been updated.  Move on.
                return ()
        else do
            -- remove the old backup and replace it with the new file.
            Log.logEvent $ "Update " ++ f ++ " with new version."
            if dryRun then
                return ()
            else do
                Dir.removeFile newFile
                Dir.copyFileWithMetadata oldFile newFile

    replaceFiles fromDir toDir fs verbose dryRun

backupDir' :: FilePath -> FilePath -> Bool -> Bool -> IO ()
backupDir' from to verbose dryRun = do
    Log.logEvent $ "*** Backup directory " ++ from ++ " ***"

    doesToDirExist <- Dir.doesPathExist to
    fromFiles <- Dir.listDirectory from
    toFiles   <- if (not doesToDirExist) && dryRun then
                    return []
                 else
                    Dir.listDirectory to
    -- was in to dir but not if from, it has been deleted.
    let delFiles = Lst.sort [ f | f <- toFiles, not $ f `elem` fromFiles  ]
    -- was in from dir, but not to dir, it was added.
        newFiles = Lst.sort [ f | f <- fromFiles, not $ f `elem` toFiles  ]
    -- in both dirs, just verify it hasn't changed.
        oldFiles = Lst.sort [ f | f <- fromFiles, f `elem` toFiles  ]

    removeFiles       to delFiles dryRun
    addFiles     from to newFiles verbose dryRun
    replaceFiles from to oldFiles verbose dryRun

    if verbose then
        Log.logEvent $ "*** Backup directory complete " ++ from ++ " ***"
    else
        return ()

-- Return value of true = ok to run backup
--      if there may be a problem, return false.
checkRootBackupDir :: FilePath -> FilePath -> IO Bool
checkRootBackupDir from to = do
    fromFiles <- Dir.listDirectory from
    toFiles   <- Dir.listDirectory to

    let delFiles = length [ f | f <- toFiles, not $ f `elem` fromFiles  ]
    -- was in from dir, but not to dir, it was added.
        newFiles = length [ f | f <- fromFiles, not $ f `elem` toFiles  ]
    -- in both dirs, just verify it hasn't changed.
        oldFiles = length [ f | f <- fromFiles, f `elem` toFiles  ]

    -- If we are going to delete files, but don't have enough old files,
    --      we could accidently be backuping up incorrectly and could
    --      end up deleting important files.
        troubleBrewing = (delFiles > 0)  &&  (oldFiles < 5)
    if troubleBrewing then do
        Cl.handleErrors [  "ABORTING BACKUP.\n"
                           ++ "        I am concerned that your backup request "
                           ++ "may cause data loss.\n        If you are sure "
                           ++ "everything is correct, you can use the --FORCE "
                           ++ "option.\n"
                        ]
        return False
    else return True

backupDir :: FilePath -> FilePath -> Bool -> Bool -> Bool -> IO ()
backupDir from to verbose dryRun force = do
    to'   <- Dir.makeAbsolute to
    from' <- Dir.makeAbsolute from

    -- place date at top of log
    Log.logDate dryRun

    -- show the backup locations
    putStr "    Backing up from: "
    putStrLn from
    putStr "    Backing up to:   "
    putStrLn to
    putStrLn ""

    -- show the parameter options
    putStr "    Settings:   "
    if verbose then putStr "VERBOSE " else return ()
    if dryRun  then putStr "DRY-RUN " else return ()
    if force   then putStr "FORCE" else return ()
    putStrLn ""
    putStrLn ""

    ok <-   if force then
                return True
            else
                checkRootBackupDir from' to'

    if ok then
        backupDir' from' to' verbose dryRun
    else
        return ()
