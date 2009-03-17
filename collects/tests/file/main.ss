#lang scheme/base

(require tests/eli-tester
         (prefix-in gzip: "gzip.ss"))

(define (tests)
  (test do (begin (gzip:tests))))

(tests)
