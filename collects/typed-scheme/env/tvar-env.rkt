#lang racket/base

;; this implements the Delta environment from the TOPLAS paper
;; (as well as every other paper on System F)

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;; technically, the mapped-to type is unnecessary, but it's convenient to have it around? maybe?

(require racket/set)
(provide (all-defined-out))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env (seteq))

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))

;; takes a list of vars
(define-syntax-rule (extend-tvars vars . body)
 (parameterize ([current-tvars (foldr (λ (v s) (set-add s v)) (current-tvars) vars)]) . body))

(define (bound-tvar? v) (set-member? (current-tvars) v))
