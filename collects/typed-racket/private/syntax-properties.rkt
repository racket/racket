#lang racket/base

(provide plambda-property)

;; TODO: make this an uninterned symbol once the phasing issue of the unit
;; tests is fixed
(define plambda-symbol 'typechecker:plambda)
(define plambda-property
  (case-lambda
    ((stx) (syntax-property stx plambda-symbol))
    ((stx value) (syntax-property stx plambda-symbol value))))
