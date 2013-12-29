#lang racket/base

(require tests/eli-tester "promise.rkt" "forcers.rkt" "lang.rkt" "space.rkt")

(module test racket/base
  (displayln "run as program for tests"))

(test do (promise-tests)
      do (forcer-tests)
      do (lang-tests)
      do (space-tests))
