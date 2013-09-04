#lang racket/base

(require tests/eli-tester
         (prefix-in gzip: "gzip.rkt")
         (prefix-in md5:  "md5.rkt")
         (prefix-in unpackers: "unpackers.rkt"))

(define (tests)
  (test do (gzip:tests)
        do (md5:tests)
        do (unpackers:tests)))

(tests)
