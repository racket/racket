#lang racket/base

(require (for-syntax racket/base))

#;
(provide (all-defined-out))

(provide honu-transformer? make-honu-transformer)

(define-values (prop:honu-transformer honu-transformer? honu-transformer-ref)
               (make-struct-type-property 'honu-transformer))

(define-values (struct:honu-trans make-honu-trans honu-trans? honu-trans-ref honu-trans-set!)
               (make-struct-type 'honu-trans #f 1 0 #f 
                                 (list (list prop:honu-transformer #t))
                                 (current-inspector) 0))

(define (make-honu-transformer proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-type-error
      'define-honu-syntax
      "procedure (arity 1)"
      proc))
  (make-honu-trans proc))

(provide (rename-out [prop:honu-operator? honu-operator?])
         make-honu-operator
         (rename-out [-honu-operator-ref honu-operator-ref]))

(define-values (prop:honu-operator prop:honu-operator? prop:honu-operator-ref)
               (make-struct-type-property 'honu-operator))

#;
(provide honu-operator?)

(define operator-fields '(precedence assocation binary unary postfix?))

(define-values (struct:honu-operator -make-honu-operator honu-operator? -honu-operator-ref honu-operator-set!)
               (make-struct-type 'honu-operator #f (length operator-fields) 0 #f 
                                 (list (list prop:honu-operator #t))
                                 (current-inspector)
                                 0))

(define (get n)
  (lambda (operator)
    (-honu-operator-ref operator n)))

(provide operator-precedence operator-association
         operator-binary-transformer operator-unary-transformer
         operator-postfix?)

(define operator-precedence (get 0))
(define operator-association (get 1))
(define operator-binary-transformer (get 2))
(define operator-unary-transformer (get 3))
(define operator-postfix? (get 4))

(define (make-honu-operator precedence associativity binary unary postfix?)
  (when (and (procedure? binary)
             (not (procedure-arity-includes? binary 2)))
    (raise-type-error
      'define-honu-operator/syntax
      "procedure (arity 2)"
      binary))
  (when (and (procedure? unary)
             (not (procedure-arity-includes? unary 1)))
    (raise-type-error
      'define-honu-operator/syntax
      "procedure (arity 1)"
      unary))
  (-make-honu-operator precedence associativity binary unary postfix?))
