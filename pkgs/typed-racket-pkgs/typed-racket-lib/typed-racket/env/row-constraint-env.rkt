#lang racket/base

;; This implements an environment that stores row
;; variable constraints for row variables.
;;
;; Since row variables are represented as ordinary
;; type variables, this additional environment is
;; needed to keep track of the constraints.

(require "../utils/utils.rkt"
         (rep type-rep))

(provide initial-row-constraint-env
         current-row-constraints
         extend-row-constraints
         has-row-constraints?
         lookup-row-constraints)

;; the initial row constraint environment - empty
(define initial-row-constraint-env '())

;; a parameter for the current type variables
(define current-row-constraints
  (make-parameter initial-row-constraint-env))

;; extend-row-constraints
;; takes a list of vars and a list of constraints
;; and extends the environment
(define-syntax-rule (extend-row-constraints vars constraints . body)
  (parameterize ([current-row-constraints
                  (extend/many (current-row-constraints)
                               vars constraints)])
    . body))

;; has-row-constraints? : Symbol -> Boolean
;; returns #t if the given variable is mapped to constraints
(define (has-row-constraints? v)
  (not (not (assq v (current-row-constraints)))))

;; lookup-row-constraints : Symbol -> Type
;; returns the mapped-to constraints or #f
(define (lookup-row-constraints var)
  (cdr (assq var (current-row-constraints))))

;; extend : Env Symbol RowConstraint -> Env
;; extend type environment with a var-constraints pair
(define (extend env var constraints)
  (cons (cons var constraints) env))

;; extend/many : Env Listof<Symbol> Listof<RowConstraint> -> Env
;; extend constraint environment for multiple variables
(define (extend/many env vars constraints)
  (for/fold ([env env])
            ([var (in-list vars)]
             [constraint (in-list constraints)])
    (extend env var constraint)))

