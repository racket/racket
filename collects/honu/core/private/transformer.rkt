#lang racket/base

(require (for-syntax racket/base))

(provide (all-defined-out))

(define-values (prop:honu-transformer honu-transformer? honu-transformer-ref)
               (make-struct-type-property 'honu-transformer))

(define-values (struct:honu-trans make-honu-trans honu-trans? honu-trans-ref honu-trans-set!)
               (make-struct-type 'honu-trans #f 1 0 #f 
                                 (list (list prop:honu-transformer #t))
                                 (current-inspector) 0))

(define (make-honu-transformer proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 2))
    (raise-type-error
      'define-honu-syntax
      "procedure (arity 2)"
      proc))
  (make-honu-trans proc))
