#lang racket/base

(provide do-lookup)
(define (do-lookup data slice)
  (cond
    [(list? data) (list-ref data slice)]
    [(string? data) (string-ref data slice)]
    [(vector? data) (vector-ref data slice)]
    [else (error 'lookup "don't know how to lookup ~a" data)]))
