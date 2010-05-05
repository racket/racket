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
                                [#:collects-path path (or/c false/c (and/c path-string? relative-path?)) #f]
                                [#:copy-collects dirs (listof path-string?) null])
         void?]{

Copies the executables in @racket[exec-files] to the directory
@racket[dest-dir], along with DLLs, frameworks, and/or shared
libraries that the executables need to run a different machine.

The arrangement of the executables and support files in
@racket[dest-dir] depends on the platform. In general
@racket[assemble-distribution] tries to do the Right Thing.

If a @racket[#:collects-path] argument is given, it overrides the
default location of the main @filepath{collects} directory for the
packaged executables. It should be relative to the @racket[dest-dir]
directory (typically inside it).

The content of each directory in the @racket[#:copy-collects] argument
is copied into the main @filepath{collects} directory for the packaged
executables.}
