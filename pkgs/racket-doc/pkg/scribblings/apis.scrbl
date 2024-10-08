#lang scribble/manual
@(require "common.rkt"
          (for-label (except-in racket/base
                                remove)
                     racket/contract/base
                     pkg
                     pkg/lib))

@title[#:tag "apis" #:style 'toc]{Package APIs}

The @racketmodname[pkg] provides a programmatic interface to the
@exec{raco pkg} commands, but additional libraries provide smaller
building blocks and local-database support.

@local-table-of-contents[]

@section{Functions for @exec{raco pkg}}

@defmodule[pkg]

The @racketmodname[pkg] module provides a programmatic interface
to the @exec{raco pkg} sub-subcommands.

 Each-long form option of the command-line interface is a keyword
 argument to the functions described below. An argument corresponding to @DFlag{type}, @DFlag{deps},
 @DFlag{format}, @DFlag{scope}, or @DFlag{multi-clone} accepts its argument as a symbol, while
 other flags that take text arguments expect strings, and flags that expect number arguments
 expect exact integers. An argument corresponding to @DFlag{scope} is also allowed to be a path string,
 as would be provided to @DFlag{scope-dir}.
 Options without argument correspond to keyword arguments that
 accept booleans, where @racket[#t] is equivalent to the presence of
 the option. When a flag can be used multiple times, its keyword-argument form
 can hold a single value, a list of values, or @racket[#f] to indicate the
 default implied by zero instances of the flag.

The parameters @racket[current-pkg-catalogs],
@racket[current-pkg-scope], @racket[current-pkg-scope-version], and
@racket[current-pkg-error] do not to affect command functions, because the
functions explicitly configure parameters based on their arguments.

@defthing[pkg-install-command procedure?]{Implements @command-ref{install}.}
@defthing[pkg-update-command procedure?]{Implements @command-ref{update}.}
@defthing[pkg-uninstall-command procedure?]{Implements @command-ref{uninstall}.}
@defthing[pkg-remove-command procedure?]{Implements @command-ref{remove}.}
@defthing[pkg-new-command procedure?]{Implements @command-ref{new}.}
@defthing[pkg-show-command procedure?]{Implements @command-ref{show}.}
@defthing[pkg-migrate-command procedure?]{Implements @command-ref{migrate}.}
@defthing[pkg-config-command procedure?]{Implements @command-ref{config}.}
@defthing[pkg-create-command procedure?]{Implements @command-ref{create}.}
@defthing[pkg-catalog-show-command procedure?]{Implements @command-ref{catalog-show}.}
@defthing[pkg-catalog-copy-command procedure?]{Implements @command-ref{catalog-copy}.}
@defthing[pkg-catalog-archive-command procedure?]{Implements @command-ref{catalog-archive}.
                                                  @history[#:added "6.0.17"]}
@defthing[pkg-archive-command procedure?]{Implements @command-ref{archive}.
                                                  @history[#:added "6.1.0.8"]}
@defthing[pkg-empty-trash-command procedure?]{Implements @command-ref{empty-trash}.
                                                  @history[#:added "6.1.1.6"]}

@include-section["lib.scrbl"]
@include-section["path.scrbl"]
@include-section["name.scrbl"]
@include-section["db.scrbl"]
@include-section["dirs-catalog.scrbl"]
@include-section["envvars.scrbl"]
