#lang scribble/doc
@(require "common.ss"
          (for-label syntax/path-spec))

@title[#:tag "path-spec"]{Resolving @scheme[include]-like Paths}

@defmodule[syntax/path-spec]

@defproc[(resolve-path-spec [path-spec-stx syntax?] 
                            [source-stx syntax?] 
                            [expr-stx syntax?] 
                            [build-path-stx syntax?]) 
         complete-path?]{

Resolves the syntactic path specification @scheme[path-spec-stx] as
for @scheme[include].

The @scheme[source-stx] specifies a syntax object whose
source-location information determines relative-path resolution.  The
@scheme[expr-stx] is used for reporting syntax errors. The
@scheme[build-path-stx] is usually @scheme[#'build-path]; it provides
an identifier to compare to parts of @scheme[path-spec-stx] to
recognize the @scheme[build-path] keyword.}
