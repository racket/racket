#lang scribble/doc
@(require "common.rkt" (for-label file/unzip))

@title[#:tag "unzip"]{@exec{zip} File Extraction}
@author{David Herman}

@defmodule[file/unzip]{The @racketmodname[file/unzip] library provides
a function to extract items from a @exec{zip} archive.}

@defproc[(unzip [in (or/c path-string? input-port?)]
                [entry-reader (cond
                                [preserve-attributes?
                                 (bytes? boolean? input-port? (and/c hash? immutable?)
                                         . -> . any)]
                                [preserve-timestamps?
                                 (bytes? boolean? input-port? (or/c #f exact-integer?)
                                         . -> . (or/c #f (-> any)))]
                                [else
                                 (bytes? boolean? input-port? . -> . any)])
                              (make-filesystem-entry-reader)]
                [#:must-unzip? must-unzip? any/c #t]
                [#:preserve-attributes? preserve-attributes? any/c #f]
                [#:preserve-timestamps? preserve-timestamps? any/c #f]
                [#:utc-timestamps? utc-timestamps? any/c #f])
         void?]{

Unzips an entire @exec{zip} archive from @racket[in]. If @racket[in]
does not start with @exec{zip}-archive magic bytes, an error is
reported only if @racket[must-unzip?] is true, otherwise the result is
@racket[(void)] with no bytes consumed from @racket[in]. If
@racket[in] is an input port and @racket[preserve-attributes?] is a
true value, it must support position setting via
@racket[file-position].

For each entry in the archive, the @racket[entry-reader] procedure is
called with three or four arguments: the byte string representing the
entry name, a boolean flag indicating whether the entry represents a
directory, an input port containing the inflated contents of the
entry, and either (if @racket[preserve-attributes?]) a hash table or
(if @racket[preserve-timestamps?] and not
@racket[preserve-attributes?]) @racket[#f] or a timestamp. The default
@racket[entry-reader] unpacks entries to the filesystem; call
@racket[make-filesystem-entry-reader] to configure aspects of the
unpacking, such as the destination directory.

When @racket[preserve-attributes?] is true, the hash table passed to
@racket[entry-reader] provides additional file attributes, and
@racket[entry-reader] must produce either @racket[#f] for a
@racket[_post-action] thunk. All @racket[_post-action] thunks are run
in order after the last call to @racket[entry-reader]; these acions
are useful for setting permissions on a directory after all contained
files are written, for eample. Attributes are mapped in the hash table
using the following keys, but either of the keys may be absent:

@itemlist[

 @item{@racket['timestamp] --- an exact integer representing the file
       timestamp}

 @item{@racket['permissions] --- an exact integer representing file
       or directory permissions}

 ]

Although @racket[preserve-attributes?] and
@racket[preserve-timestamps?] provide extra information to
@racket[entry-reader], unpacking entries and preserving attributes and
timestamps is up to @racket[entry-reader]. The reader produced by
@racket[make-filesystem-entry-reader] preserves whatever information
is it given, except for directories on Windows or directories that already exist, and it
returns a @racket[_post-action] thunk only when given a directory plus
a timestamp and/or permission attribute.

For timestamps, @exec{zip} archives normally record modification dates in local time,
but if @racket[utc-timestamps?] is true, then the time in the archive
is interpreted as UTC.

When @racket[preserve-attributes?] is @racket[#f], then @racket[in] is
read in a single pass as long as file entries are found. Beware that
if the input represents an archive that has file entries not
referenced by the ``central directory'' in the archive, the
corresponding files are unpacked, anyway.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:preserve-timestamps?] argument.}
         #:changed "6.0.1.12" @elem{Added the @racket[#:utc-timestamps?] argument.}
         #:changed "8.0.0.10" @elem{Added the @racket[#:must-unzip?] argument.}
         #:changed "8.2.0.7" @elem{Changed the @racket[#:must-unzip?] default to @racket[#t].}
         #:changed "8.7.0.9" @elem{Added the @racket[#:preserve-attributes?] argument.}]}


@defproc[(call-with-unzip [in (or/c path-string? input-port?)]
                          [proc (-> path-string? any)]
                          [#:must-unzip? must-unzip? any/c #t])
         any]{

Unpacks @racket[in] to a temporary directory, calls @racket[proc] on
the temporary directory's path, and then deletes the temporary
directory while returning the result of @racket[proc].

Like @racket[unzip], no error is reported in the case @racket[in] is
not a @exec{zip} archive, unless @racket[must-unzip?] is true.

@history[#:added "6.0.1.6"
         #:changed "8.0.0.10" @elem{Added the @racket[#:must-unzip?] argument.}
         #:changed "8.2.0.7" @elem{Changed the @racket[#:must-unzip?] default to @racket[#t].}]}


@defproc[(make-filesystem-entry-reader
          [#:dest dest-path (or/c path-string? #f) #f]
          [#:strip-count strip-count exact-nonnegative-integer? 0]
          [#:permissive? permissive? any/c #f]
          [#:exists exists (or/c 'skip 'error 'replace 'truncate 
                                 'truncate/replace 'append 'update
                                 'can-update 'must-truncate)
                           'error])
         ((bytes? boolean? input-port?) ((or/c hash? #f exact-integer?))
          . ->* . (or/c void? #f (-> void?)))]{

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

Unless @racket[permissive?] is true, then entries with paths containing
an up-directory indicator are disallowed, and a link entry whose target
is an absolute path or contains an up-directory indicator is also
disallowed. Absolute paths are always disallowed. A disallowed
path triggers an exception.

If @racket[exists] is @racket['skip] and the file for an entry already
exists, then the entry is skipped. Otherwise, @racket[exists] is
passed on to @racket[open-output-file] for writing the entry's
inflated content.

When the resulting returned procedure is called, it will produce
@racket[(void)] unless it is given a hash table as a fourth argument.
When given a hash table, the result is either @racket[#f] or a thunk.
A thunk is returned on Unix and Mac OS when arguments refer to a
directory that does not already exist and either a timestamp
attribute, permission attribure, or both are provided.

@history[#:changed "6.0.0.3"
         @elem{Added support for the optional timestamp argument in the result function.}
         #:changed "6.3"
         @elem{Added the @racket[#:permissive?] argument.}
         #:changed "8.7.0.9"
         @elem{Added support for an optional attributes hash-table argument in the result function.}]}


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


@defproc[(unzip-entry [in (or/c path-string? input-port?)]
                      [zipdir zip-directory?]
                      [entry (or/c bytes? path-string?)]
                      [entry-reader (cond
                                      [preserve-attributes?
                                       (bytes? boolean? input-port? (and/c hash? immutable?)
                                               . -> . any)]
                                      [preserve-timestamps?
                                       (bytes? boolean? input-port? (or/c #f exact-integer?)
                                               . -> . any)]
                                      [else
                                       (bytes? boolean? input-port? . -> . any)])
                                    (make-filesystem-entry-reader)]
                      [#:preserve-attributes? preserve-attributes? any/c #f]
                      [#:preserve-timestamps? preserve-timestamps? any/c #f]
                      [#:utc-timestamps? utc-timestamps? any/c #f])
         (if preserve-attributes? void? (or/c #f (-> any)))]{

Unzips a single entry from a @exec{zip} archive based on a previously
read @tech{zip directory}, @racket[zipdir], from
@racket[read-zip-directory].  If @racket[in] is an input port, it must
support position setting via @racket[file-position].

The @racket[entry] parameter is a byte string whose name must be found
in the zip file's central directory. If @racket[entry] is not a byte
string, it is converted using @racket[path->zip-path].

The @racket[entry-reader] argument is used to read the contents of the
zip entry in the same way as for @racket[unzip]. When
@racket[preserve-attributes?] is a true value, the result of
@racket[entry-reader] is returned by @racket[unzip-entry], and it will
be either @racket[#f] or a @racket[_post-action] thunk. The returned
@racket[_post-action] thunks should all be called after extracting
from @racket[in] is complete.

If @racket[entry] is not in @racket[zipdir], an
@racket[exn:fail:unzip:no-such-entry] exception is raised.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:preserve-timestamps?] argument.}
         #:changed "6.0.1.12" @elem{Added the @racket[#:utc-timestamps?] argument.}
         #:changed "8.7.0.9" @elem{Added the @racket[#:preserve-attributes?] argument.}]}


@defproc[(call-with-unzip-entry [in (or/c path-string? input-port?)]
                                [entry path-string?]
                                [proc (-> path-string? any)])
         any]{

Unpacks @racket[entry] within @racket[in] to a temporary directory,
calls @racket[proc] on the unpacked file's path, and then
deletes the temporary directory while returning the result of
@racket[proc].

@history[#:added "6.0.1.6"]}


@defproc[(path->zip-path [path path-string?]) bytes?]{

Converts a file name potentially containing path separators in the current
platform's format to use path separators recognized by the zip file
format: @litchar{/}.}


@defstruct[(exn:fail:unzip:no-such-entry exn:fail) ([entry bytes?])]{

Raised when a requested entry cannot be found in a @exec{zip}
archive. The @racket[entry] field is a byte string representing the
requested entry name.}
