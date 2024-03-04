# PROBLEM

Currently unusable as files are moved before being fully downloaded.

# File-sorter

A tool for sorting a directory into more managable sub-directories.

Running file-sorter on a directory starts a service that watches for 'added file events'.
On such an event, the added file is moved into an appropriate sub-directory.
Unless modified by a configuration file this is a directory matching the file extension.

## Config
Where files are moved can be customized by a configuration file. 
```
zip -> ignore
png -> images
jpg -> images
txt -> docs
pdf -> docs
```

Here ignore is a keyword such that any file with the extension zip will be
ignored.
png, jpg are moved to a sub-directory named images/
txt, pdf are moved to a sub-directory named docs/

If the target directory does not exist it will be created.

## Safe

If there are name clashes then the file being moved will be renamed accordingly.
For example, assume `$WATCHEDDIR/pdfs/assignment.pdf` exist already, then when
`assignment.pdf` is added to `$WATCHEDDIR`, `assignment.pdf` will be moved to
`$WATCHEDDIR/pdfs/assignment(1).pdf`.

## Existing files

Existing files can be sorted by providing the flag `(-s|--sort-once)`.
This will not make `file-sorter` watch a directory, but rather sort the
directory and all its files into sub-directories.
