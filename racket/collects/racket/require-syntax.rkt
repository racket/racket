#lang racket/base

(provide define-require-syntax
         (for-syntax syntax-local-require-introduce))

(require (for-syntax racket/base
                     syntax/apply-transformer
                     "require-transform.rkt"))

(define-for-syntax (syntax-local-require-introduce x)
  (unless (syntax? x)
    (raise-argument-error 'syntax-local-introduce-require "syntax?" x))
  (syntax-local-introduce x))

(define-for-syntax (make-require-macro proc)
  (make-require-transformer
   (lambda (stx)
     (expand-import (local-apply-transformer proc stx 'expression)))))

(define-syntax (define-require-syntax stx)
  (syntax-case stx ()
    [(_ id proc)
     (identifier? #'id)
     (syntax/loc stx
       (define-syntax id
         (make-require-macro proc)))]
    [(_ (id . args) . body)
     (identifier? #'id)
     (syntax/loc stx
       (define-require-syntax id
         (lambda args . body)))]))
