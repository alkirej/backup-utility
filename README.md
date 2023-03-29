# BACKUP UTILITY
Backup utility I use for my media server.

It requires the user to supply a "from" directory and a "to" directory.
The utility will examine the 2 directories and the associated tree of
files and subdirectories.

When completed, if no errors were encountered, the to directory tree will be 
identical to the from directory.

This means new or changed files will be updated and that files in the to
directory that are not found in the from directory will be deleted.

This goal of this utility is to be as light weight as possible. This means
that similar files in the from and to directories are skipped. (This is
determined by the modified date and file size.)

The goal of this utility is NOT to be able to retreive old copies of files
that may have become inaccessible or corrupted.

# COMMAND LINE USAGE
<pre>
backup-util -f &lt;DIR&gt; -t &lt;DIR&gt; [-v]
  -f DIR  --from=DIR  location of original data
  -t DIR  --to=DIR    location to store backup data
  -v      --verbose   chatty output on stderr
          --FORCE     force backup despite possible trouble
  -d      --dry-run   dry run of backup
</pre>

# NOTES
Use of the -d (dry-run) flag will execute the backup without making any changes
to your directory structure.  It is recommended to use this flag before executing 
a backup and closely examining the results to avoid data loss.

This utility was developed and tested using Ubuntu Linux.

The utility was developed in Haskell.

# BUILD
If you have a properly setup Haskell development environment including cabal, you can build
this program simply by executing:
<pre>cabal build</pre>

The utility will be found within the **dist-newstyle** directory tree.
