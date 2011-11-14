#lang racket/base

(require (for-syntax racket/base))

(provide (rename-out [prop:fixture? fixture?])
         make-fixture
         (rename-out [-fixture-ref fixture-ref]))
(define-values (prop:fixture prop:fixture? prop:fixture-ref)
               (make-struct-type-property 'fixture))

#;
(provide honu-operator?)
(define-values (struct:fixture -make-fixture fixture? -fixture-ref fixture-set!)
               (make-struct-type 'fixture #f 1 0 #f 
                                 (list (list prop:fixture #t))
                                 (current-inspector) 0))

(define (make-fixture transformer)
  (when (and (procedure? transformer)
             (not (procedure-arity-includes? transformer 2)))
    (raise-type-error
      'define-fixture
      "procedure (arity 2)"
      transformer))
  (-make-fixture transformer))

