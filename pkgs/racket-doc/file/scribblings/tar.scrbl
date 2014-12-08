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
              [#:exists-ok? exists-ok? any/c #f]
              [#:path-prefix path-prefix (or/c #f path-string?) #f]
              [#:get-timestamp get-timestamp
                               (path? . -> . exact-integer?)
                               (if timestamp
                                   (lambda (p) timestamp)
                                   file-or-directory-modify-seconds)])
         exact-nonnegative-integer?]{

Creates @racket[tar-file], which holds the complete content of all
@racket[path]s.  The given @racket[path]s are all expected to be
relative paths for existing directories and files (i.e., relative
to the current directory).  If a nested path is provided as a
@racket[path], its ancestor directories are also added to the
resulting tar file, up to the current directory (using
@racket[pathlist-closure]).

If @racket[exists-ok?] is @racket[#f], then an exception is raised if
@racket[tar-file] exists already. If @racket[exists-ok?] is true, then
@racket[tar-file] is truncated or replaced if it exists already.

If @racket[path-prefix] is not @racket[#f], then it is prefixed to
each path in the archive.

The @racket[get-timestamp] function is used to obtain the modification
date to record in the archive for each file or directory.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.1.1.1" @elem{Added the @racket[#:exists-ok?] argument.}]}


@defproc[(tar->output [paths (listof path?)]
                      [out output-port? (current-output-port)]
                      [#:path-prefix path-prefix (or/c #f path-string?) #f]
                      [#:get-timestamp get-timestamp
                                       (path? . -> . exact-integer?)
                                       (if timestamp
                                           (lambda (p) timestamp)
                                           file-or-directory-modify-seconds)])
         exact-nonnegative-integer?]{

Packages each of the given @racket[paths] in a @exec{tar} format
archive that is written directly to the @racket[out].  The specified
@racket[paths] are included as-is (except for adding @racket[path-prefix], if any); if a directory is specified, its
content is not automatically added, and nested directories are added
without parent directories.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}]}


@defproc[(tar-gzip [tar-file path-string?]
                   [paths path-string?] ...
                   [#:exists-ok? exists-ok? any/c #f]
                   [#:path-prefix path-prefix (or/c #f path-string?) #f]
                   [#:get-timestamp get-timestamp
                                    (path? . -> . exact-integer?)
                                    (if timestamp
                                        (lambda (p) timestamp)
                                        file-or-directory-modify-seconds)])
         void?]{

Like @racket[tar], but compresses the resulting file with @racket[gzip].

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.1.1.1" @elem{Added the @racket[#:exists-ok?] argument.}]}
