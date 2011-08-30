#lang scheme/base

(require (for-template racket/base "defined-checks.rkt"))
(provide make-term-fn
         term-fn?
         term-fn-get-id
         (struct-out term-id)
         (struct-out judgment-form)
         judgment-form-id?
         (struct-out defined-term)
         defined-term-id?
         defined-check
         not-expression-context)

(define-values (struct-type make-term-fn term-fn? term-fn-get term-fn-set!) 
  (make-struct-type 'term-fn #f 1 0))
(define term-fn-get-id (make-struct-field-accessor term-fn-get 0))

(define-struct term-id (id depth))

(define ((transformer-predicate p?) stx)
  (and (identifier? stx)
       (cond [(syntax-local-value stx (λ () #f)) => p?]
             [else #f])))

(define-struct judgment-form (name mode proc lang lws))
(define judgment-form-id? 
  (transformer-predicate judgment-form?))

(define-struct defined-term (value))
(define defined-term-id?
  (transformer-predicate defined-term?))

(define (defined-check id desc #:external [external id])
  (if (eq? (identifier-binding id) 'lexical)
      (quasisyntax/loc external (check-defined-lexical #,id '#,external #,desc))
      (quasisyntax/loc external (check-defined-module (λ () #,id) '#,external #,desc))))

(define (not-expression-context stx)
  (when (eq? (syntax-local-context) 'expression)
    (raise-syntax-error #f "not allowed in an expression context" stx)))