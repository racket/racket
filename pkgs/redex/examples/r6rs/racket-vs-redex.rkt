#lang racket/base

#|

This runs (most of) the R6RS test suite in both Racket and Redex
and prints out timing results that compare the two of them.

It skips a few tests that are testing internal states of the redex
r6rs model since they use features that aren't in r6rs itself.

|#

(require "r6rs-tests.rkt")
(main #:compare-with-racket? #t)
