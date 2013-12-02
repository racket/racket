#lang info

(define name "Typed Racket Test Suite")

;; No need to compile the actual integration tests, just the harness.
(define compile-omit-paths
  '("typed-racket/succeed"
    "typed-racket/fail"
    "typed-racket/xfail"
    "typed-racket/optimizer" ;; FIXME: should be improved by stamourv
    "typed-racket/tr-random-testing.rkt" ;; Requires recompiling all of redex when rebuilding TR tests
    ))
