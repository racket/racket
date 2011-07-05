#lang racket
(require (for-syntax racket/struct-info racket/match))

(define-signature sig2^
  ((struct my-error (v))))


(define-unit a-unit@
  (import)
  (export sig2^)
  (define-struct my-error (v)))

(define-values/invoke-unit/infer a-unit@)

(begin-for-syntax
  struct-info?
  (match (extract-struct-info (syntax-local-value #'my-error))
    [(list str m1 m? sels sets _)
     (unless (= (length sels) (length sets))
       (error "not the same number of selectors and setters"))]))
