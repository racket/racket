#lang racket/base

(provide do-lookup)
(define (do-lookup data slice)
  (list-ref data slice))
