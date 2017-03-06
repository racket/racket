#lang racket/base
(require "performance-hint.rkt")
(provide nan?
         infinite?
         positive-integer?
         negative-integer?
         nonpositive-integer?
         nonnegative-integer?
         natural?)

;; these are broken out from racket/math
;; so that racket/contract can depend on them

(begin-encourage-inline

  ;; real predicates
  (define (nan? x)
    (unless (real? x) (raise-argument-error 'nan? "real?" x))
    (or (eqv? x +nan.0) (eqv? x +nan.f)))

  (define (infinite? x)
    (unless (real? x) (raise-argument-error 'infinite? "real?" x))
    (or (= x +inf.0) (= x -inf.0)))

  (define (positive-integer? x)
    (and (integer? x) (positive? x)))

  (define (negative-integer? x)
    (and (integer? x) (negative? x)))

  (define (nonpositive-integer? x)
    (and (integer? x) (not (positive? x))))

  (define (nonnegative-integer? x)
    (and (integer? x) (not (negative? x))))

  (define (natural? x)
    (exact-nonnegative-integer? x)))