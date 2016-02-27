#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          (for-label racket/gui
                     compiler/distribute
                     launcher/launcher))

@title{API for Distributing Executables}

@defmodule[compiler/distribute]{

The @racketmodname[compiler/distribute] library provides a function to
perform the same work as @exec{raco distribute}.}


@defproc[(assemble-distribution [dest-dir path-string?]
                                [exec-files (listof path-string?)]
                                [#:executables? executables? any/c #t]
                                [#:relative-base relative-base (or/c path-string? #f) #f]
                                [#:collects-path path (or/c false/c (and/c path-string? relative-path?)) #f]
                                [#:copy-collects dirs (listof path-string?) null])
         void?]{

Copies the executables in @racket[exec-files] to the directory
@racket[dest-dir], along with DLLs, frameworks, shared libraries,
and/or runtime files that the executables need to run a different
machine. If @racket[executables?] is @racket[#f], then the
@racket[exec-files] are treated as plain data files, instead of
executables, and they are modified in-place.

The arrangement of the executables and support files in
@racket[dest-dir] depends on the platform. In general,
@racket[assemble-distribution] tries to do the Right Thing, but a
non-@racket[#f] value for @racket[relative-base] specifies a
path for reaching the assembled content relative to the executable at
run time. When @racket[executables?] is @racket[#f], then the default
access path is @racket[dest-dir], with its relativeness preserved.

If a @racket[#:collects-path] argument is given, it overrides the
default location of the main @filepath{collects} directory for the
packaged executables. It should be relative to the @racket[dest-dir]
directory (typically inside it).

The content of each directory in the @racket[#:copy-collects] argument
is copied into the main @filepath{collects} directory for the packaged
executables.

@history[#:changed "6.3" @elem{Added the @racket[#:executables?]
                                      and @racket[#:relative-base] arguments.}]}
