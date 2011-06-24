#lang scribble/doc
@(require "common.rkt" (for-label file/tar file/gzip))

@title[#:tag "tar"]{@exec{tar} File Creation}

@defmodule[file/tar]{The @racketmodname[file/tar] library provides
utilities to create archive files in USTAR format, like the archive
that the Unix utility @exec{pax} generates.  The USTAR format imposes
limits on path lengths.  The resulting archives contain only
directories and files (symbolic links are followed), and owner
information is not preserved; the owner that is stored in the archive
is always ``root.''}

@defproc[(tar [tar-file path-string?][path path-string?] ...) 
         exact-nonnegative-integer?]{

Creates @racket[tar-file], which holds the complete content of all
@racket[path]s.  The given @racket[path]s are all expected to be
relative path names of existing directories and files (i.e., relative
to the current directory).  If a nested path is provided as a
@racket[path], its ancestor directories are also added to the
resulting tar file, up to the current directory (using
@racket[pathlist-closure]).}

@defproc[(tar->output [paths (listof path?)]
                      [out output-port? (current-output-port)])
         exact-nonnegative-integer?]{

Packages each of the given @racket[paths] in a @exec{tar} format
archive that is written directly to the @racket[out].  The specified
@racket[paths] are included as-is; if a directory is specified, its
content is not automatically added, and nested directories are added
without parent directories.}

@defproc[(tar-gzip [tar-file path-string?] [paths path-string?] ...)
         void?]{

Like @racket[tar], but compresses the resulting file with @racket[gzip].
}
