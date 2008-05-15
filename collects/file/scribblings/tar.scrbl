#lang scribble/doc
@(require "common.ss"
          (for-label file/tar))

@title[#:tag "tar"]{@exec{tar} File Creation}

@defmodule[file/tar]{The @schememodname[file/tar] library provides
utilities to create archive files in USTAR format, like the archive
that the Unix utility @exec{pax} generates.  The USTAR format imposes
limits on path lengths.  The resulting archives contain only
directories and files (symbolic links are followed), and owner
information is not preserved; the owner that is stored in the archive
is always ``root.''}

@defproc[(tar [tar-file path-string?][path path-string?] ...) 
         void?]{

Creates @scheme[tar-file], which holds the complete content of all
@scheme[path]s.  The given @scheme[path]s are all expected to be
relative path names of existing directories and files (i.e., relative
to the current directory).  If a nested path is provided as a
@scheme[path], its ancestor directories are also added to the
resulting tar file, up to the current directory (using
@scheme[pathlist-closure]).}

@defproc[(tar->output [paths (listof path-string?)]
                      [out output-port? (current-output-port)])
         void?]{

Packages each of the given @scheme[paths] in a @exec{tar} format
archive that is written directly to the @scheme[out].  The specified
@scheme[paths] are included as-is; if a directory is specified, its
content is not automatically added, and nested directories are added
without parent directories.}
