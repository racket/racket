#lang scribble/doc

@(require scribble/manual)

@title{Testing} 

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


@(require scribble/eval
          (for-label racket/contract
                     racket/class
                     racket/gui/base
                     lang/posn
                     lang/imageeq
                     lang/prim))

@(define (htdp-ref s) @secref[#:doc '(lib "scribblings/htdp-langs/htdp-langs.scrbl") s])

