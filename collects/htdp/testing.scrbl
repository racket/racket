#lang scribble/doc

@(require scribble/manual
          (for-label test-engine/racket-tests))

@title{Testing} 

@; -----------------------------------------------------------------------------
@defmodule[htdp/testing #:use-sources (test-engine/racket-tests)]

The library re-exports the following identifiers from test-engine/racket-tests:

@defproc[(build-test-engine) void?]
@defproc[(builder) void?]
@defproc[(display-results) void?]
@defproc[(error-handler) void?]
@defproc[(exn:fail:wish) void?]
@defproc[(generate-report) void?]
@defproc[(get-test-engine) void?]
@defproc[(reset-tests) void?]
@defproc[(run-tests) void?]
@defproc[(scheme-test-data) void?]
@defproc[(signature-test-info%) void?]


@(require scribble/eval
          (for-label racket/contract
                     racket/class
                     racket/gui/base
                     lang/posn
                     lang/imageeq
                     lang/prim))

@(define (htdp-ref s) @secref[#:doc '(lib "scribblings/htdp-langs/htdp-langs.scrbl") s])

