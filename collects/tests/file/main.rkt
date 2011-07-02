#lang scheme/base

(require tests/eli-tester
         (prefix-in gzip: "gzip.rkt")
         (prefix-in md5:  "md5.rkt"))

(define (tests)
  (test do (begin (gzip:tests) (md5:tests))))

(tests)
