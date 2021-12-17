#lang racket/base
(require "scope.rkt")

(provide add-space-scope
         remove-space-scope)

(define (add-space-scope stx space)
  (if space
      (add-scope stx (make-interned-scope space))
      stx))

(define (remove-space-scope stx space)
  (if space
      (remove-scope stx (make-interned-scope space))
      stx))
