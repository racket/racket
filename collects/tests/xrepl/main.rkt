#lang at-exp racket/base

(require "xrepl.rkt" "wrapping-output.rkt" "known-module.rkt" tests/eli-tester)
(test do (test-wrapping-output)
      do (test-known-module)
      do (test-xrepl))
