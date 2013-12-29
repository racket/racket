#lang at-exp racket/base

(module test racket/base
  (displayln "run as program for tests"))

(require "xrepl.rkt" "wrapped-output.rkt" "known-module.rkt" tests/eli-tester)
(test do (test-wrapped-output)
      do (test-known-module)
      do (test-xrepl))
