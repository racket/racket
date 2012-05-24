#lang racket/base

(require tests/eli-tester
         (prefix-in gzip: "gzip.rkt")
         (prefix-in md5:  "md5.rkt"))

(define (tests)
  (test do (gzip:tests)
        do (md5:tests)))

(tests)
