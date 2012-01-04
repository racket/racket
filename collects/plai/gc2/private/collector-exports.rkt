#lang scheme
(require (for-syntax racket/syntax))
(provide (all-defined-out))

(define-syntax (define-collector-export stx)
  (syntax-case stx ()
    [(_ i)
     (with-syntax 
      ([collector:i (format-id #'i "collector:~a" #'i)]
       [set-collector:i! (format-id #'i "set-collector:~a!" #'i)])
      #'(begin (define collector:i false)
               (define (set-collector:i! proc)
                 (set! collector:i proc))))]))

(define-syntax-rule (define-collector-exports i ...)
  (begin (define-collector-export i)
         ...))

(define-collector-exports
  deref
  alloc-flat
  cons
  first
  rest
  flat?
  cons?
  set-first!
  set-rest!
  closure
  closure?
  closure-code-ptr
  closure-env-ref)
