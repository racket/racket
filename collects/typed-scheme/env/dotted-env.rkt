#lang racket/base

(require "type-env-structs.rkt" syntax/id-table)
(provide (all-defined-out))

;; this environment maps lexical identifiers to pairs of types and bounds
;; bounds are type variables which must be bound with ...
;; bounds are represented as symbols

;; the environment for types of ... variables
(define dotted-env (make-parameter (make-empty-env (make-immutable-free-id-table))))

;; run code in an extended dotted env
(define-syntax with-dotted-env/extend
  (syntax-rules ()
    [(_ i t v . b) (parameterize ([dotted-env (extend (dotted-env) i (cons t v))]) . b)]))