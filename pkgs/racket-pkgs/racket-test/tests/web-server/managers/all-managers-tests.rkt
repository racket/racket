#lang racket/base
(require rackunit)
(provide all-managers-tests)

(define all-managers-tests
  (test-suite
   "Continuation Managers"
   
   ; XXX test timeout.rkt
   ; XXX test none.rkt
   ; XXX test lru.rkt
   ))
