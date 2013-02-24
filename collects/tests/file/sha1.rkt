#lang racket/base
(require file/sha1 tests/eli-tester)

(provide tests)

(module+ main (tests))
(define (tests)
  (test
   ;; The docs say that sha1 must return a 40-character string,
   ;; and should include leading zeros.
   (string-length (sha1 (open-input-string ""))) => 40
   (string-length (sha1 (open-input-string " r a c k et"))) => 40))
