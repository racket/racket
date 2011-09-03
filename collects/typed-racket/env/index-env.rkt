#lang racket/base

;; this implements the Theta environment from the TOPLAS paper

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;; technically, the mapped-to type is unnecessary, but it's convenient to have it around? maybe?

(require racket/require racket/set (path-up "utils/tc-utils.rkt"))
(provide (all-defined-out))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-index-env (seteq))

;; a parameter for the current type variables
(define current-indexes (make-parameter initial-index-env))

;; takes a single index
(define-syntax-rule (extend-indexes index . body)
 (parameterize ([current-indexes (set-add (current-indexes) index)]) . body))

(define (bound-index? v) (set-member? (current-indexes) v))

(define (infer-index stx)
  (define bounds (set-map (current-indexes) values))
  (when (null? bounds)
    (tc-error/stx stx "No type variable bound with ... in scope for ... type"))
  (unless (null? (cdr bounds))
    (tc-error/stx stx "Cannot infer bound for ... type"))
  (car bounds))
