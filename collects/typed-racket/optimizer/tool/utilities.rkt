#lang racket/base

;; TODO all this stuff should make it into unstable/X

(provide (all-defined-out))

(define (regexp-filter r log)
  (for/list ([l (in-list log)] #:when (regexp-match r l))
    l))
