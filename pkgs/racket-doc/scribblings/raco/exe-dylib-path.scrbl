#lang scribble/manual
@(require "common.rkt"
          (for-label racket/base
                     racket/contract
                     compiler/exe-dylib-path))

@title[#:tag "exe-dylib-path"]{Mac OS X Dynamic Library Paths}

@defmodule[compiler/exe-dylib-path]{The
@racketmodname[compiler/exe-dylib-path] library provides functions for
reading and adjusting dynamic-library references in a Mac OS X
executable.}

@history[#:added "6.3"]

@defproc[(find-matching-library-path [exe-path path-string?]
                                     [library-str string?])
         (or/c #f string?)]{

Searches dynamic-linking information in @racket[exe-path] for a
library reference whose name includes @racket[library-str] and returns
the executable's path to the library for the first match. If no match is
found, the result is @racket[#f].}

@defproc[(update-matching-library-path [exe-path path-string?]
                                       [library-str string?]
                                       [library-path-str string?])
         void?]{

Searches dynamic-linking information in @racket[exe-path] for each
library reference whose name includes @racket[library-str] and replaces
the executable's path to that library with @racket[library-path-str].

A single match is expected, and the update assumes enough space for
the new path, perhaps because the executable is linked with
@Flag{headerpad_max_install_names}.}
