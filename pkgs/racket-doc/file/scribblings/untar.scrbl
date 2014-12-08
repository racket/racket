#lang scribble/doc
@(require "common.rkt" (for-label file/untar))

@title[#:tag "untar"]{@exec{tar} File Extraction}

@defmodule[file/untar]{The @racketmodname[file/untar] library provides
a function to extract items from a TAR/USTAR archive.}

@defproc[(untar           [in (or/c path-string? input-port?)]
                          [#:dest dest-path (or/c path-string? #f) #f]
                          [#:strip-count strip-count exact-nonnegative-integer? 0]
                          [#:filter filter-proc
                                    (path? (or/c path? #f)
                                     symbol? exact-integer? (or/c path? #f)
                                     exact-nonnegative-integer?
                                     exact-nonnegative-integer?
                                     . -> . any/c)
                                    (lambda args #t)])
         void?]{

Extracts TAR/USTAR content from @racket[in].

If @racket[dest-path] is not @racket[#f], every path in the archive is
prefixed to determine the destination path of the extracted item.

If @racket[strip-count] is positive, then @racket[strip-count] path
elements are removed from the item path from the archive (before
prefixing the path with @racket[dest-path]); if the item's path
contains @racket[strip-count] elements, then it is not extracted.

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
not unpacked.}
