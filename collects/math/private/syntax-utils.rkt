#lang racket/base

(require (for-syntax racket/base)
         (for-template racket)
         racket/syntax)

(provide (all-defined-out)
         (all-from-out (submod "." ensures)))

(define-syntax-rule (define-inline-op name inline-op typed-op inline-pats ...)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(_ . inline-pats)  (syntax/loc stx (inline-op . inline-pats))] ...
      [(_ . args)  (syntax/loc stx (typed-op . args))]
      [_  (syntax/loc stx typed-op)])))

(module ensures racket/base
  (require racket/flonum
           typed/racket/base)
  
  (provide (all-defined-out))

  (define-syntax-rule (ensure-index name n-expr)
    (let: ([n : Integer  n-expr])
      (if (index? n) n (raise-argument-error name "Index" n))))
  
  (define-syntax-rule (ensure-flvector name xs-expr)
    (let: ([xs : FlVector  xs-expr])
      (if (flvector? xs) xs (raise-argument-error name "FlVector" xs))))
  
  (define-syntax-rule (ensure-procedure name f-expr T)
    (let: ([f : T  f-expr])
      (if (procedure? f) f (raise-argument-error name "Procedure" f))))
  
  )

(require 'ensures)
