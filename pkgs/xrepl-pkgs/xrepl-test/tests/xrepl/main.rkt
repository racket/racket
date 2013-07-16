#lang at-exp racket/base

(require "xrepl.rkt" "wrapped-output.rkt" "known-module.rkt" tests/eli-tester)
(test do (test-wrapped-output)
      do (test-known-module)
      do (test-xrepl))
