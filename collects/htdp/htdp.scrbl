#lang scribble/doc

@(require scribble/manual (for-label scheme))

@title[#:style '(toc) #:tag "htdp"]{How to Design HtDP TeachPacks}

@section{What are TeachPacks}

TeachPacks for "How to Design Programs" provide functionality that extends
 the teaching languages for specific exercises. The extensions fit smoothly
 to the teaching languages so that students don't get nonsensical error
 messages or undocumented language features through the backdoor.

@; -----------------------------------------------------------------------------
@section{Errors} 

@defmodule[htdp/error]

To provide uniform error messages from the TeachPacks, this module
provides several functions: 

@defproc[(check-arg) void?]{
 }

@defproc[(check-arity) void?]{
 }

@defproc[(check-proc) void?]{
 }

@defproc[(check-result) void?]{
 }

@defproc[(check-list-list) void?]{
 }

@defproc[(check-color) void?]{
 }

@defproc[(check-fun-res) void?]{
 }

@defproc[(check-dependencies) void?]{
 }

@defproc[(natural?) void?]{
 }

@defproc[(find-non) void?]{
 }
 
@defproc[(tp-exn?) void?]{
 }

@defproc[(number->ord) void?]{
 }

@section{Testing} 

@; -----------------------------------------------------------------------------
@defmodule[htdp/testing #:use-sources (test-engine/scheme-tests)]

The library re-exports the following identifiers from test-engine/scheme-tests:

 @racket[build-test-engine]
 @racket[builder]
 @racket[display-results]
 @racket[error-handler]
 @racket[exn:fail:wish]
 @racket[generate-report]
 @racket[get-test-engine]
 @racket[reset-tests]
 @racket[run-tests]
 @racket[scheme-test-data]
 @racket[signature-test-info%]
