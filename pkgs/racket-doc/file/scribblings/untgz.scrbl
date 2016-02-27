#lang scribble/doc
@(require "common.rkt" (for-label file/untgz
                                  file/untar
                                  file/gunzip))

@title[#:tag "untgz"]{@exec{tar}+@exec{gzip} File Extraction}

@defmodule[file/untgz]{The @racketmodname[file/untgz] library provides
a function to extract items from a possible @exec{gzip}ped TAR/USTAR archive.}

@defproc[(untgz           [in (or/c path-string? input-port?)]
                          [#:dest dest-path (or/c path-string? #f) #f]
                          [#:strip-count strip-count exact-nonnegative-integer? 0]
                          [#:permissive? permissive? any/c #f]
                          [#:filter filter-proc
                                    (path? (or/c path? #f)
                                     symbol? exact-integer? (or/c path? #f)
                                     exact-nonnegative-integer?
                                     exact-nonnegative-integer?
                                     . -> . any/c)
                                    (lambda args #t)])
         void?]{

The same as @racket[untar], but if @racket[in] is in @exec{gzip} form,
it is @racket[gunzip]ped as it is unpacked.

@history[#:changed "6.3" @elem{Added the @racket[#:permissive?] argument.}]}
