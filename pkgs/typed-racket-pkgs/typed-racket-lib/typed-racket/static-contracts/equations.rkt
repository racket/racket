#lang racket

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

; add-equation! (equation-set? var? (-> value?))
(define (add-equation! eqs var thunk)
  (hash-set! (equation-set-equations eqs) var thunk))

(define current-variable-values (make-parameter (hash)))

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
  (hash-ref (current-variable-values) v))
