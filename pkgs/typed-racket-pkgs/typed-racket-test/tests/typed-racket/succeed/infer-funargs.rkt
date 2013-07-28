#lang typed-scheme

(define-struct: Environment
 ((parent : (Option Environment))
  (bindings : (HashTable Symbol Integer)))
 #:mutable)

(: make-empty-env (case-lambda [-> Environment]
                              [Environment -> Environment]))
(define make-empty-env
 (case-lambda:
  [() (make-Environment #f (make-hasheq))]
  [((parent : Environment)) (make-Environment parent (make-hasheq))]))
