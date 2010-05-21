#lang racket/base

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;; technically, the mapped-to type is unnecessary, but it's convenient to have it around? maybe?

(require "type-env-structs.rkt")
(provide (all-defined-out))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env (make-empty-env #hasheq()))

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))