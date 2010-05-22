#lang scheme

(require "checks.ss"
         "../require-provide.ss")

(provide require-provide-suite)

(define require-provide-suite
  (test-suite "require-provide.ss"))
