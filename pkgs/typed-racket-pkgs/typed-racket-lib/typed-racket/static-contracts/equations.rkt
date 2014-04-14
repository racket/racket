#lang racket/base

;; Manages a set of mutually recursive equations, and provids functionality for finding a fix point.
;; An equation set has two components
;;  1. a mapping of variables to initial values.
;;  2. a mapping of variables to thunks that compute new values.
;; 
;; Variables are an opaque structure, which support accessing their current value.


(provide
  make-equation-set
  add-variable!
  add-equation!
  resolve-equations
  variable-ref)


(struct var ())

; equations: (hash/c var? (-> value?))
; initial-values: (hash/c var? (-> value?))
(struct equation-set (equations initial-values))

(define (make-equation-set)
  (equation-set (make-hash) (make-hash)))

; add-variable!: (equation-set? value? -> var?)
(define (add-variable! eqs initial-value)
  (define a-var (var))
  (hash-set! (equation-set-initial-values eqs) a-var initial-value)
  a-var)

; add-equation!: (equation-set? var? (-> value?) -> void?)
(define (add-equation! eqs var thunk)
  (hash-set! (equation-set-equations eqs) var thunk))

(define current-variable-values (make-parameter (hash)))

;; resolve-equations (equation-set? -> (hash/c var? value?))
;; Produces a mapping of variables to values such that every equation holds.
(define (resolve-equations eqs)
  (define values (hash-copy (equation-set-initial-values eqs)))
  (parameterize ((current-variable-values values))
    (let loop ()
      (define change #f) 
      (for (((v thunk) (equation-set-equations eqs)))
        (define new-value (thunk))
        (define old-value (hash-ref values v))
        (unless (equal? new-value old-value)
          (set! change #t)
          (hash-set! values v new-value)))
      (when change
        (loop)))
    values))

(define (variable-ref v)
  (hash-ref (current-variable-values) v (lambda () (error 'variable-ref "No value available."))))
