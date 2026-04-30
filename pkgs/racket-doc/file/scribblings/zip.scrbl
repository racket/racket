#lang scribble/doc
@(require "common.rkt" (for-label file/zip file/gzip file/gunzip scheme/file))

@title[#:tag "zip"]{@exec{zip} File Creation}

@defmodule[file/zip]{The @racketmodname[file/zip] library provides
utilities to create @exec{zip} archive files, which are compatible
with both Windows and Unix (including Mac OS) unpacking. The actual
compression is implemented by @racket[deflate].}

@defproc[(zip [zip-file path-string?] [path-or-entry (or/c path-string? zip-entry?)] ...
              [#:level level exact-integer? 6]
              [#:timestamp timestamp (or/c #f exact-integer?) #f]
              [#:get-timestamp get-timestamp
                               (path? . -> . exact-integer?)
                               (if timestamp
                                   (lambda (p) timestamp)
                                   file-or-directory-modify-seconds)]
              [#:utc-timestamps? utc-timestamps? any/c #f]
              [#:round-timestamps-down? round-timestamps-down? any/c #f]
              [#:path-prefix path-prefix (or/c #f path-string?) #f]
              [#:system-type sys-type symbol? (system-type)])
         void?]{

Creates @racket[zip-file], which holds the complete content of all
@racket[path-or-entry]s. Each @racket[path-or-entry] is either a path
that refers to an existing file or directory on the filesystem, or it
is a @racket[zip-entry] that can override the compression level for
that entry or describe an entry without needing corresponding content
on the filesystem.

The given paths among @racket[path-or-entry]s are all expected to be
relative path names of existing directories and files (i.e., relative
to the current directory). If a nested path is provided as a plain
path, its ancestor directories are also added to the resulting
@exec{zip} file, up to the current directory (using
@racket[pathlist-closure]). A @racket[zip-entry] is included as given,
so it can be used to control the order of entries and their
compression within the archive, including entries whose contents are
provided as byte strings or input ports.

Files are packaged as usual for
@exec{zip} files, including permission bits for both Windows and Unix
(including Mac OS).  The permission bits are determined by
@racket[file-or-directory-permissions], which does not preserve the
distinction between owner/group/other permissions. Also, symbolic
links are always followed.

The @racket[get-timestamp] function is used to obtain the modification
date to record in the archive for a file or directory. Normally,
@exec{zip} archives record modification dates in local time, but if
@racket[utc-timestamps?] is true, then the UTC time is recorded.
Timestamps in @exec{zip} archives are precise only to two seconds; by
default, the time is rounded toward the future (like WinZip or PKZIP),
but time is rounded toward the past (like Java) if
@racket[round-timestamps-down?]  is true.

The @racket[sys-type] argument determines the system type recorded in
the archive.

If @racket[path-prefix] is not @racket[#f], then it prefixes the name
of each path as it is written in the @exec{zip} file, and directory
entries are added for each element of @racket[path-prefix].

The @racket[level] argument controls compression from @racket[0] to
@racket[9]. A level of @racket[0] stores file entries without ZIP
compression, while higher levels write Deflate-compressed entries.

This interface can be used for formats like EPUB, where specific files
must appear first in the archive and be stored without compression.

@history[#:changed "6.0.0.3"
         @elem{Added the @racket[#:get-timestamp] and @racket[#:system-type] arguments.}
         #:changed "6.0.1.12"
         @elem{Added the @racket[#:path-prefix], @racket[#:utc-timestamps?], and 
                @racket[#:utc-timestamps-down?] arguments.}
         #:changed "9.2.0.2"
         @elem{Added support for @racket[zip-entry] arguments.}]}
         


@defproc[(zip->output [paths-and-entries (listof (or/c path-string? zip-entry?))]
                      [out output-port? (current-output-port)]
                      [#:level level exact-integer? 6]
                      [#:timestamp timestamp (or/c #f exact-integer?) #f]
                      [#:get-timestamp get-timestamp
                                       (path? . -> . exact-integer?)
                                       (if timestamp
                                           (lambda (p) timestamp)
                                           file-or-directory-modify-seconds)]
                      [#:utc-timestamps? utc-timestamps? any/c #f]
                      [#:round-timestamps-down? round-timestamps-down? any/c #f]
                      [#:path-prefix path-prefix (or/c #f path-string?) #f]
                      [#:system-type sys-type symbol? (system-type)])
         void?]{

Like @racket[zip], but packages each element of the given
@racket[paths-and-entries] in a @exec{zip} file written directly to
@racket[out]. The specified @racket[paths-and-entries] are included
as-is (except for adding @racket[path-prefix], if any); if a
directory is specified, its content is not automatically added, and
nested directories are added without parent directories.

As with @racket[zip], a @racket[level] of @racket[0] stores file
entries without ZIP compression, while higher levels write
Deflate-compressed entries.

@history[#:changed "6.0.0.3"
         @elem{Added the @racket[#:get-timestamp] and @racket[#:system-type] arguments.}
         #:changed "6.0.1.12"
         @elem{Added the @racket[#:path-prefix], @racket[#:utc-timestamps?], and 
                @racket[#:utc-timestamps-down?] arguments.}
         #:changed "9.2.0.2"
         @elem{Added support for @racket[zip-entry] arguments.}]}


@defproc*[([(make-zip-entry [path (and/c path-string? relative-path?)]
                            [level exact-integer?])
            zip-entry?]
           [(make-zip-entry [kind (or/c 'path 'file 'directory)]
                            [path (and/c path-string? relative-path?)]
                            [content (or/c input-port? (-> input-port?) bytes? #f)]
                            [size (or/c #f exact-nonnegative-integer?)]
                            [attribs (hash/c symbol? any/c)]
                            [level exact-integer?])
            zip-entry?])]{

Creates a @racket[zip-entry] value.

The two-argument form is a shorthand for an existing filesystem path
whose archive representation should use the given compression
@racket[level].

The six-argument form is analogous to @racket[tar-entry]:

@itemlist[
 @item{@racket[kind] is one of @racket['path], @racket['file], or
       @racket['directory].}
 @item{@racket[path] is always a relative archive path.}
 @item{If @racket[kind] is @racket['path], then @racket[path] names an
       existing filesystem path, and @racket[content] and @racket[size]
       are ignored.}
 @item{If @racket[kind] is @racket['file], then @racket[path] names
       the archive entry, and @racket[content] supplies the bytes using
       an input port, a thunk that produces an input port, or a byte
       string. If @racket[size] is not @racket[#f], then it must match
       the number of bytes supplied by @racket[content].}
 @item{If @racket[kind] is @racket['directory], then @racket[path]
       names the directory entry, and @racket[content] is expected to
       be @racket[#f].}
 @item{The @racket[attribs] hash can contain @racket['permissions] and
       @racket['modify-seconds] to override the default metadata that is
       recorded in the archive.}
 @item{@racket[level] controls compression using the same
       @racket[0]-to-@racket[9] convention as @racket[zip]. A level of
       @racket[0] stores the entry without ZIP compression, while higher
       levels write Deflate-compressed entries.}]

This form makes it possible to create ZIP archives directly from
in-memory content without constructing a corresponding directory tree
on the filesystem.
}


@defstruct*[zip-entry ([kind (or/c 'path 'file 'directory)]
                       [path (and/c path-string? relative-path?)]
                       [content (or/c input-port? (-> input-port?) bytes? #f)]
                       [size (or/c #f exact-nonnegative-integer?)]
                       [attribs (hash/c symbol? any/c)]
                       [level exact-integer?])
             #:omit-constructor]{

Represents a ZIP entry to be included by @racket[zip] or
@racket[zip->output].

@history[#:added "9.2.0.2"]}


@defboolparam[zip-verbose on?]{

A parameter that controls output during a @racket[zip]
operation. Setting this parameter to a true value causes @racket[zip]
to display to @racket[(current-error-port)] the filename that is
currently being compressed.}
