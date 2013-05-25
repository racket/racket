#lang scribble/doc
@(require "common.rkt" (for-label syntax/path-spec))

@title[#:tag "path-spec"]{Resolving @racket[include]-like Paths}

@defmodule[syntax/path-spec]

@defproc[(resolve-path-spec [path-spec-stx syntax?] 
                            [source-stx syntax?] 
                            [expr-stx syntax?] 
                            [build-path-stx syntax?]) 
         complete-path?]{

Resolves the syntactic path specification @racket[path-spec-stx] as
for @racket[include].

The @racket[source-stx] specifies a syntax object whose
source-location information determines relative-path resolution.  The
@racket[expr-stx] is used for reporting syntax errors. The
@racket[build-path-stx] is usually @racket[#'build-path]; it provides
an identifier to compare to parts of @racket[path-spec-stx] to
recognize the @racket[build-path] keyword.}
