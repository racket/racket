#lang scribble/doc
@(require "common.rkt" (for-label file/tar file/gzip racket/file))

@title[#:tag "tar"]{@exec{tar} File Creation}

@defmodule[file/tar]{The @racketmodname[file/tar] library provides
utilities to create archive files in USTAR format, like the archive
that the Unix utility @exec{pax} generates.  The USTAR format imposes
limits on path lengths.  The resulting archives contain only
directories, files, and symbolic links, and owner
information is not preserved; the owner that is stored in the archive
is always ``root.''

Symbolic links (on Unix and Mac OS X) are not followed, and the path
in a link must be less than 100 bytes.}

@defproc[(tar [tar-file path-string?]
              [path path-string?] ...
              [#:path-prefix path-prefix (or/c #f path-string?) #f])
         exact-nonnegative-integer?]{

Creates @racket[tar-file], which holds the complete content of all
@racket[path]s.  The given @racket[path]s are all expected to be
relative paths for existing directories and files (i.e., relative
to the current directory).  If a nested path is provided as a
@racket[path], its ancestor directories are also added to the
resulting tar file, up to the current directory (using
@racket[pathlist-closure]).

If @racket[path-prefix] is not @racket[#f], then it is prefixed to
each path in the archive.}

@defproc[(tar->output [paths (listof path?)]
                      [out output-port? (current-output-port)]
                      [#:path-prefix path-prefix (or/c #f path-string?) #f])
         exact-nonnegative-integer?]{

Packages each of the given @racket[paths] in a @exec{tar} format
archive that is written directly to the @racket[out].  The specified
@racket[paths] are included as-is (except for adding @racket[path-prefix], if any); if a directory is specified, its
content is not automatically added, and nested directories are added
without parent directories.}

@defproc[(tar-gzip [tar-file path-string?]
                   [paths path-string?] ...
                   [#:path-prefix path-prefix (or/c #f path-string?) #f])
         void?]{

Like @racket[tar], but compresses the resulting file with @racket[gzip].
}
