#lang scheme/base

(require tests/eli-tester
         (prefix-in gzip: "gzip.ss")
         (prefix-in md5:  "md5.ss"))

(define (tests)
  (test do (begin (gzip:tests) (md5:tests))))

(tests)
