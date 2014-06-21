#lang racket/base
(require racket/contract/base)
(provide ihash/c mhash/c)
(define (ihash/c k v) (and/c (hash/c k v) immutable?))
(define (mhash/c k v) (and/c (hash/c k v) (not/c immutable?)))
