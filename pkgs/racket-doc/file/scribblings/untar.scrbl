#lang scribble/doc
@(require "common.rkt" (for-label file/untar
                                  (only-in file/tar tar-entry)))

@title[#:tag "untar"]{@exec{tar} File Extraction}

@defmodule[file/untar]{The @racketmodname[file/untar] library provides
a function to extract items from a TAR/USTAR archive using GNU and/or
pax extensions to support long pathnames.}

@defproc[(untar           [in (or/c path-string? input-port?)]
                          [#:dest dest-path (or/c path-string? #f) #f]
                          [#:strip-count strip-count exact-nonnegative-integer? 0]
                          [#:permissive? permissive? any/c #f]
                          [#:filter filter-proc
                                    (path? (or/c path? #f)
                                     symbol? exact-integer? (or/c path? #f)
                                     exact-nonnegative-integer?
                                     exact-nonnegative-integer?
                                     . -> . any/c)
                                    (lambda args #t)]
                          [#:handle-entry handle-entry
                                          ((or/c 'file 'directory 'link)
                                           (and path? relative-path?)
                                           (or/c input-port? #f path?)
                                           exact-nonnegative-integer?
                                           (hash/c symbol? any/c)
                                           . -> . (listof (-> any)))
                                          handle-tar-entry])
         void?]{

Extracts TAR/USTAR content from @racket[in], recognizing
POSIX.1-2001/pax and GNU extensions for long paths and long
symbolic-link targets.

If @racket[dest-path] is not @racket[#f], every path in the archive is
prefixed to determine the destination path of the extracted item.

If @racket[strip-count] is positive, then @racket[strip-count] path
elements are removed from the item path from the archive (before
prefixing the path with @racket[dest-path]); if the item's path
contains @racket[strip-count] elements, then it is not extracted.

Unless @racket[permissive?] is true, then archive items with paths containing
an up-directory indicator are disallowed, and a link item whose target
is an absolute path or contains an up-directory indicator is also
disallowed. Absolute paths are always disallowed. A disallowed
path triggers an exception.

For each item in the archive, @racket[filter-proc] is applied to

@itemlist[

 @item{the item's path as it appears in the archive;}

 @item{a destination path that is based on the path in the archive,
       @racket[strip-count], and @racket[dest-path]--which can be
       @racket[#f] if the item's path does not have
       @racket[strip-count] or more elements;}

 @item{a symbol representing the item's type---@racket['file],
       @racket['dir], @racket['link], @racket['hard-link],
       @racket['character-special], @racket['block-special],
       @racket['fifo], @racket['contiguous-file],
       @racket['extended-header], @racket['extended-header-for-next],
       or @racket['unknown]---where only @racket['file],
       @racket['dir], or @racket['link] can be unpacked by
       @racket[untar];}

 @item{an exact integer representing the item's size;}

 @item{a target path for a @racket['link] type or @racket[#f]
       for any other type;}

 @item{an integer representing the item's modification date; and}

 @item{an integer representing the item's permissions}

]

If the result of @racket[filter-proc] is @racket[#f], then the item is
not unpacked.

The @racket[handle-entry] function is called to unpack one entry, and
the default @racket[handle-tar-entry] function for
@racket[handle-entry] creates a directory, file, or link on the
filesystem. The @racket[handle-entry] function must accept five
arguments:
@;
@itemlist[

 @item{@racket[_kind] --- one of @racket['file], @racket['directory],
       or @racket['link].}

 @item{@racket[_path] --- the relative path recorded in the TAR file.}

 @item{@racket[_content] --- an input port that provides the content
       for a @racket['file] entry, where exactly @racket[_size] bytes
       must be read from the port before @racket[handle-entry]
       returns. For a @racket['directory] entry, @racket[_content] is
       @racket[#f]. For a @racket['link] entry, @racket[_content] is
       a path for the link target.}

 @item{@racket[_size] --- the number of bytes for a @racket['file]
       entry, and @racket[0] for other entries.}

 @item{@racket[_attribs] --- an immutable hash table mapping symbols
       to attribute values. The available keys may change, but the
       currently included keys are the same ones as recognized in
       @racket[tar-entry].}

]

The result of @racket[handle-entry] is a list of thunks that are
called in order after the TAR input is fully unpacked. A result thunk
from @racket[handle-entry] is useful, for example, to set a
directory's modification time after all files have been written to it.

@history[#:changed "6.3" @elem{Added the @racket[#:permissive?] argument.}
         #:changed "6.7.0.4" @elem{Support long paths and long symbolic-link
                                   targets using POSIX.1-2001/pax and GNU
                                   extensions.}
         #:changed "8.1.0.5" @elem{Added the @racket[#:handle-entry] argument.}]}


@defproc[(handle-tar-entry [kind (or/c 'file 'directory 'link)]
                           [path (and path? relative-path?)]
                           [content (or/c input-port? #f path?)]
                           [size exact-nonnegative-integer?]
                           [attribs (hash/c symbol? any/c)])
         (listof (-> any))]{

As the default entry handler for @racket[untar],
@racket[handle-tar-entry] creates directories and files and returns a
list of thunks that complete unpacking by setting directory
permissions and modification times.

@history[#:added "8.1.0.5"]}
