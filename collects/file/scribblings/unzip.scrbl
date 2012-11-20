#lang scribble/doc
@(require "common.rkt" (for-label file/unzip))

@title[#:tag "unzip"]{@exec{zip} File Extraction}
@author{David Herman}

@defmodule[file/unzip]{The @racketmodname[file/unzip] library provides
a function to extract items from a @exec{zip} archive.}

@defproc[(unzip [in (or/c path-string? input-port)]
                [entry-reader (bytes? boolean? input-port? . -> . any)
                              (make-filesystem-entry-reader)])
         void?]{

Unzips an entire @exec{zip} archive from @racket[in].

For each entry in the archive, the @racket[entry-reader] procedure is
called with three arguments: the byte string representing the entry
name, a boolean flag indicating whether the entry represents a
directory, and an input port containing the inflated contents of the
entry. The default @racket[entry-reader] unpacks entries to the
filesystem; call @racket[make-filesystem-entry-reader] to configure
aspects of the unpacking, such as  the destination directory.}

@defproc[(make-filesystem-entry-reader
          [#:dest dest-path (or/c path-string? #f) #f]
          [#:strip-count strip-count exact-nonnegative-integer? 0]
          [#:exists exists (or/c 'skip 'error 'replace 'truncate 
                                 'truncate/replace 'append 'update
                                 'can-update 'must-truncate)
                           'error])
         (bytes? boolean? input-port? . -> . any)]{

Creates a @exec{zip} entry reader that can be used with either
@racket[unzip] or @racket[unzip-entry] and whose behavior is to save
entries to the local filesystem. Intermediate directories are always
created if necessary before creating files. Directory entries are
created as directories in the filesystem, and their entry contents are
ignored.

If @racket[dest-path] is not @racket[#f], every path in the archive is
prefixed to determine the destination path of the extracted entry.

If @racket[strip-count] is positive, then @racket[strip-count] path
elements are removed from the entry path from the archive (before
prefixing the path with @racket[dest-path]); if the item's path
contains @racket[strip-count] elements, then it is not extracted.

If @racket[exists] is @racket['skip] and the file for an entry already
exists, then the entry is skipped. Otherwise, @racket[exists] is
passed on to @racket[open-output-file] for writing the entry's
inflated content.}


@defproc[(read-zip-directory [in (or/c path-string? input-port?)]) zip-directory?]{

Reads the central directory of a @exec{zip} file and generates a
@deftech{zip directory} representing the zip file's contents.  If
@racket[in] is an input port, it must support position setting via
@racket[file-position].

This procedure performs limited I/O: it reads the list of entries from
the @exec{zip} file, but it does not inflate any of their
contents.}

@defproc[(zip-directory? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{zip directory},
@racket[#f] otherwise.}


@defproc[(zip-directory-entries [zipdir zip-directory?]) (listof bytes?)]{

Extracts the list of entries for a @exec{zip} archive.}


@defproc[(zip-directory-contains? [zipdir zip-directory?]
                                  [name (or/c bytes? path-string?)])
         boolean?]{

Determines whether the given entry name occurs in the given @tech{zip
directory}. If @racket[name] is not a byte string, it is converted
using @racket[path->zip-path].

Directory entries match with or without trailing slashes.}


@defproc[(zip-directory-includes-directory? [zipdir zip-directory?]
                                            [name (or/c bytes? path-string?)])
         boolean?]{

Determines whether the given name is included anywhere in the given
@tech{zip directory} as a filesystem directory, either as an entry
itself or as the containing directory of other entries. If
@racket[name] is not a byte string, it is converted using
@racket[path->zip-path].}


@defproc[(unzip-entry [path (or/c path-string? input-port?)]
                      [zipdir zip-directory?]
                      [entry (or/c bytes? path-string?)]
                      [entry-reader (bytes? boolean? input-port? . -> . any)
                                    (make-filesystem-entry-reader)])
         void?]{

Unzips a single entry from a @exec{zip} archive based on a previously
read @tech{zip directory}, @racket[zipdir], from
@racket[read-zip-directory].  If @racket[in] is an input port, it must
support position setting via @racket[file-position].

The @racket[entry] parameter is a byte string whose name must be found
in the zip file's central directory. If @racket[entry] is not a byte
string, it is converted using @racket[path->zip-path].

The @racket[read-entry] argument is used to read the contents of the zip entry
in the same way as for @racket[unzip].

If @racket[entry] is not in @racket[zipdir], an
@racket[exn:fail:unzip:no-such-entry] exception is raised.}


@defproc[(path->zip-path [path path-string?]) bytes?]{

Converts a file name potentially containing path separators in the current
platform's format to use path separators recognized by the zip file
format: @litchar{/}.}


@defstruct[(exn:fail:unzip:no-such-entry exn:fail) ([entry bytes?])]{

Raised when a requested entry cannot be found in a @exec{zip}
archive. The @racket[entry] field is a byte string representing the
requested entry name.}
