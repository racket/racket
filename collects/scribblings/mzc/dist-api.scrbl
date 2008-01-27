#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          (for-label scheme/gui
                     compiler/distribute
                     launcher/launcher))

@title{Scheme API for Distributing Executables}

@defmodule[compiler/distribute]{

The @schememodname[compiler/distribute] library provides a function to
perform the same work as @exec{mzc --exe} or @exec{mzc --gui-exe}.}


@defproc[(assemble-distribution [dest-dir path-string?]
                                [exec-files (listof path-string?)]
                                [#:collects-path path (or/c false/c (and/c path-string? relative-path?)) #f]
                                [#:copy-collects dirs (listof path-string?) null])
         void?]{

Copies the executables in @scheme[exec-files] to the directory
@scheme[dest-dir], along with DLLs, frameworks, and/or shared
libraries that the executables need to run a different machine.

The arrangement of the executables and support files in
@scheme[dest-dir] depends on the platform. In general
@scheme[assemble-distribution] tries to do the Right Thing.

If a @scheme[#:collects-path] argument is given, it overrides the
default location of the main @filepath{collects} directory for the
packaged executables. It should be relative to the @scheme[dest-dir]
directory (typically inside it).

The content of each directory in the @scheme[#:copy-collects] argument
is copied into the main @filepath{collects} directory for the packaged
executables.}
