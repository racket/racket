#lang racket/base

(provide define-provide-syntax
         (for-syntax syntax-local-provide-introduce))

(require (for-syntax racket/base
                     syntax/apply-transformer
                     "provide-transform.rkt"))

(define-for-syntax (syntax-local-provide-introduce x)
  (unless (syntax? x)
    (raise-argument-error 'syntax-local-introduce-provide "syntax?" x))
  (syntax-local-introduce x))

(define-for-syntax (make-provide-macro proc)
  (make-provide-transformer
   (lambda (stx modes)
     (expand-export (local-apply-transformer proc stx 'expression) modes))))

(define-syntax (define-provide-syntax stx)
  (syntax-case stx ()
    [(_ id proc)
     (identifier? #'id)
     (syntax/loc stx
       (define-syntax id
         (make-provide-macro proc)))]
    [(_ (id . args) . body)
     (identifier? #'id)
     (syntax/loc stx
       (define-provide-syntax id
         (lambda args . body)))]))
