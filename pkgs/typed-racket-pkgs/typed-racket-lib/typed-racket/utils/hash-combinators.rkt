#lang racket/base
(require racket/contract/base)
(provide ihash/c mhash/c)
(define (ihash/c k v) (hash/c k v #:immutable #t))
(define (mhash/c k v) (hash/c k v #:immutable #f))
