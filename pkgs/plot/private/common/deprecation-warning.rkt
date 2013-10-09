#lang racket/base

(require "parameters.rkt")

(provide deprecation-warning)

(define warnings (make-hash))

(define (deprecation-warning name [replacement-name #f])
  (when (plot-deprecation-warnings?)
    (hash-ref! warnings name
               (λ ()
                 (if replacement-name
                     (eprintf "~a is deprecated and may be removed in the future; use ~a instead~n"
                              name replacement-name)
                     (eprintf "~a is deprecated and may be removed in the future"
                              name))
                 #t))))
