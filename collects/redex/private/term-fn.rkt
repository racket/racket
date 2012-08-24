#lang racket/base

(require (for-template racket/base "defined-checks.rkt"))
(provide make-term-fn
         term-fn?
         term-fn-get-id
         (struct-out term-id)
         
         (struct-out judgment-form)
         
         (struct-out defined-term)
         defined-term-id?
         defined-check
         not-expression-context
         make-language-id
         language-id-nts
         pattern-symbols)

(define-values (struct-type make-term-fn term-fn? term-fn-get term-fn-set!) 
  (make-struct-type 'term-fn #f 1 0))
(define term-fn-get-id (make-struct-field-accessor term-fn-get 0))

(define-struct term-id (id depth))

(define (transformer-predicate p? stx)
  (and (identifier? stx)
       (cond [(syntax-local-value stx (λ () #f)) => p?]
             [else #f])))

(define-struct judgment-form (name mode proc mk-proc lang lws rule-names))

(define-struct defined-term (value))
(define (defined-term-id? stx)
  (transformer-predicate defined-term? stx))

(define (defined-check id desc #:external [external id])
  (if (eq? (identifier-binding id) 'lexical)
      (quasisyntax/loc external (check-defined-lexical #,id '#,external #,desc))
      (quasisyntax/loc external (check-defined-module (λ () #,id) '#,external #,desc))))

(define (not-expression-context stx)
  (when (eq? (syntax-local-context) 'expression)
    (raise-syntax-error #f "not allowed in an expression context" stx)))

(define-values (language-id make-language-id language-id? language-id-get language-id-set) (make-struct-type 'language-id #f 2 0 #f '() #f 0))

(define (language-id-nts stx id) (language-id-getter stx id 1))
(define (language-id-getter stx id n)
  (unless (identifier? stx)
    (raise-syntax-error id "expected an identifier defined by define-language" stx))
  (let ([val (syntax-local-value stx (λ () #f))])
    (unless (and (set!-transformer? val)
                 (language-id? (set!-transformer-procedure val)))
      (raise-syntax-error id "expected an identifier defined by define-language" stx))
    (language-id-get (set!-transformer-procedure val) n)))

(define pattern-symbols '(any number natural integer real string variable 
                              variable-not-otherwise-mentioned hole symbol))
