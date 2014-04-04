#lang racket/base

(require (for-syntax racket/base) racket/unsafe/ops
         "utils.rkt")

(provide (all-defined-out))

;; Returns the interpolated distance of z from za toward zb
;; Examples: if z = za, this returns 0.0
;;           if z = zb, this returns 1.0
;;           if z = (za + zb) / 2, this returns 0.5
;; Intuitively, regard a use (solve-t z za zb) as "the point between za and zb".
(define-syntax-rule (solve-t z za zb)
  (/ (- z za) (- zb za)))

(define-syntax-rule (unsafe-solve-t z za zb)
  (unsafe-fl/ (unsafe-fl- z za) (unsafe-fl- zb za)))

(define-syntax-rule (unsafe-unsolve-t za zb t)
  (unsafe-fl+ (unsafe-fl* t zb) (unsafe-fl* (unsafe-fl- 1.0 t) za)))

(define-syntax-rule (unsolve-t za zb t)
  (cond [(eq? t 0)  za]
        [(eq? t 1)  zb]
        [else  (+ (* t zb) (* (- 1 t) za))]))

(define-syntax-rule (unsafe-flavg4 z1 z2 z3 z4)
  (unsafe-fl* 0.25 (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ z1 z2) z3) z4)))

(define-syntax-rule (all pred? obj ...) (and (pred? obj) ...))

(define-syntax (find-failure-index stx)
  (syntax-case stx ()
    [(_ pred? id ...)
     (with-syntax ([(i ...)   (build-list (length (syntax->list #'(id ...))) values)])
       (syntax/loc stx
         (or (and (not (pred? id)) i) ...)))]))
