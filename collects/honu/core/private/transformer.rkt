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
(define-values (struct:honu-operator -make-honu-operator honu-operator? -honu-operator-ref honu-operator-set!)
               (make-struct-type 'honu-operator #f 4 0 #f 
                                 (list (list prop:honu-operator #t))
                                 (current-inspector) 0))

(define (make-honu-operator precedence associativity binary unary)
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
  (-make-honu-operator precedence associativity binary unary))
