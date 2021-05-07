#lang scribble/doc
@(require "common.rkt" (for-label file/tar file/gzip racket/file))

@title[#:tag "tar"]{@exec{tar} File Creation}

@defmodule[file/tar]{The @racketmodname[file/tar] library provides
utilities to create archive files in USTAR format, like the archive
that the Unix utility @exec{pax} generates. Long paths are supported
using either the POSIX.1-2001/pax or GNU format for long paths. The
resulting archives contain only directories, files, and symbolic
links, and owner information is not preserved; the owner that is
stored in the archive is always ``root.''

Symbolic links (on Unix and Mac OS) are not followed by default.}


@defproc[(tar [tar-file path-string?]
              [path-or-entry (or/c path-string? tar-entry?)] ...
              [#:follow-links? follow-links? any/c #f]
              [#:exists-ok? exists-ok? any/c #f]
              [#:format format (or/c 'pax 'gnu 'ustar) 'pax]
              [#:path-prefix path-prefix (or/c #f path-string?) #f]
              [#:path-filter path-filter (or/c #f (path? . -> . any/c)) #f]
              [#:timestamp timestamp (or/c #f exact-integer?) #f]
              [#:get-timestamp get-timestamp
                               (path? . -> . exact-integer?)
                               (if timestamp
                                   (lambda (p) timestamp)
                                   file-or-directory-modify-seconds)])
         exact-nonnegative-integer?]{

Creates @racket[tar-file], which holds the complete content of all
@racket[path-or-entry]s. Each @racket[path-or-entry] is either a path
that refers to a file, directory, or link on the filesystem, or it is
a @racket[tar-entry] that describes such an entity without requiring
it to exist on the filesystem.

The given paths among @racket[path-or-entry]s are all expected to be
relative paths for existing directories and files (i.e., relative
to the current directory for a @racket[path-or-entry] is a path).  If a nested path is provided in a
@racket[path-or-entry], its ancestor directories are also added to the
resulting tar file, up to the current directory (using
@racket[pathlist-closure]). If @racket[follow-links?] is false, then
symbolic links are included in the resulting tar file as links.

If @racket[exists-ok?] is @racket[#f], then an exception is raised if
@racket[tar-file] exists already. If @racket[exists-ok?] is true, then
@racket[tar-file] is truncated or replaced if it exists already.

The @racket[format] argument determines the handling of long paths and
long symbolic-link targets. If @racket[format] is @racket['pax], then
POSIX.1-2001/pax extensions are used. If @racket[format] is
@racket['gnu], then GNU extensions are used. If @racket[format] is
@racket['ustar], then @racket[tar] raises an error for too-long paths
or symbolic-link targets.

If @racket[path-prefix] is not @racket[#f], then it is prefixed to
each path in the archive.

The @racket[get-timestamp] function is used to obtain the modification
date to record in the archive for each file or directory.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.1.1.1" @elem{Added the @racket[#:exists-ok?] argument.}
         #:changed "6.3.0.3" @elem{Added the @racket[#:follow-links?] argument.}
         #:changed "6.3.0.11" @elem{Added the @racket[#:path-filter] argument.}
         #:changed "6.7.0.4" @elem{Added the @racket[#:format] argument and
                                   effectively changed its default from @racket['ustar]
                                   to @racket['pax].}
         #:changed "7.3.0.3" @elem{Added the @racket[#:timestamp] argument.}
         #:changed "8.1.0.5" @elem{Added support for @racket[tar-entry] arguments.}]}


@defproc[(tar->output [paths-and-entries (listof (or/c path? tar-entry?))]
                      [out output-port? (current-output-port)]
                      [#:follow-links? follow-links? any/c #f]
                      [#:format format (or/c 'pax 'gnu 'ustar) 'pax]
                      [#:path-prefix path-prefix (or/c #f path-string?) #f]
                      [#:path-filter path-filter (or/c #f (path? . -> . any/c)) #f]
                      [#:timestamp timestamp (or/c #f exact-integer?) #f]
                      [#:get-timestamp get-timestamp
                                       (path? . -> . exact-integer?)
                                       (if timestamp
                                           (lambda (p) timestamp)
                                           file-or-directory-modify-seconds)])
         exact-nonnegative-integer?]{

Like @racket[tar], but packages each element of the given @racket[paths-and-entries] in a @exec{tar} format
archive that is written directly to the @racket[out].  The specified
@racket[paths-and-entries] are included as-is (except for adding @racket[path-prefix], if any); if a directory is specified, its
content is not automatically added, and nested directories are added
without parent directories.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.3.0.3" @elem{Added the @racket[#:follow-links?] argument.}
         #:changed "6.3.0.11" @elem{Added the @racket[#:path-filter] argument.}
         #:changed "6.7.0.4" @elem{Added the @racket[#:format] argument and
                                   effectively changed its default from @racket['ustar]
                                   to @racket['pax].}
         #:changed "7.3.0.3" @elem{Added the @racket[#:timestamp] argument.}
         #:changed "8.1.0.5" @elem{Added support for @racket[tar-entry] arguments.}]}


@defproc[(tar-gzip [tar-file path-string?]
                   [paths-and-entries (and/c path-string? tar-entry?)] ...
                   [#:follow-links? follow-links? any/c #f]
                   [#:exists-ok? exists-ok? any/c #f]
                   [#:format format (or/c 'pax 'gnu 'ustar) 'pax]
                   [#:path-prefix path-prefix (or/c #f path-string?) #f]
                   [#:timestamp timestamp (or/c #f exact-integer?) #f]
                   [#:get-timestamp get-timestamp
                                    (path? . -> . exact-integer?)
                                    (if timestamp
                                        (lambda (p) timestamp)
                                        file-or-directory-modify-seconds)])
         void?]{

Like @racket[tar], but compresses the resulting file with @racket[gzip].

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.1.1.1" @elem{Added the @racket[#:exists-ok?] argument.}
         #:changed "6.3.0.3" @elem{Added the @racket[#:follow-links?] argument.}
         #:changed "6.7.0.4" @elem{Added the @racket[#:format] argument and
                                   effectively changed its default from @racket['ustar]
                                   to @racket['pax].}
         #:changed "7.3.0.3" @elem{Added the @racket[#:timestamp] argument.}
         #:changed "8.1.0.5" @elem{Added support for @racket[tar-entry] arguments.}]}


@defstruct[tar-entry ([kind (or/c 'file 'directory 'link)]
                      [path (and/c path-string? relative-path?)]
                      [content (or/c input-port? (-> input-port?) #f path-string?)]
                      [size exact-nonnegative-integer?]
                      [attribs (hash/c symbol? any/c)])]{

Represents a file, directory, or link to be included in a USTAR file
or stream.

If @racket[kind] is @racket['file], then @racket[content] must be an
input port or a thunk that produces an input port, and it must provide
exactly @racket[size] bytes. If @racket[kind] is @racket['directory],
then @racket[content] and @racket[size] are expected to be @racket[#f]
and @racket[0]. If @racket[kind] is @racket['link], then
@racket[content] must be a path, and @racket[size] is expected to be
@racket[0].

The @racket[attribs] field contains a hash table providing additional
properties of the entry. The following keys are currently used when
writing a USTAR file or stream:

       @itemlist[

        @item{@racket['permissions] --- an integer representing read,
              write, and execute permissions in the form accepted by
              @racket[file-or-directory-permissions].}

        @item{@racket['modify-seconds] --- an integer representing a
              modification time, which is consistent with
              @racket[file-or-directory-modify-seconds].}

        @item{@racket['owner] --- an exact integer presenting a file
             owner ID.}

        @item{@racket['owner-bytes] --- a byte string representing a
             file owner name.}

        @item{@racket['group] --- an exact integer presenting a file
             group ID.}

        @item{@racket['group-bytes] --- a byte string representing a
             file group name.}

       ]

@history[#:added "8.1.0.5"]}
