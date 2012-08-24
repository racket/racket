#lang typed/racket/base

(require "private/functions/log1p.rkt"
         "private/functions/expm1.rkt"
         "private/functions/gamma.rkt"
         "private/functions/log-factorial.rkt"
         "private/functions/log-gamma.rkt"
         "private/functions/log-arithmetic.rkt"
         "private/functions/incomplete-gamma.rkt")

(provide (all-from-out
          "private/functions/log1p.rkt"
          "private/functions/expm1.rkt"
          "private/functions/gamma.rkt"
          "private/functions/log-factorial.rkt"
          "private/functions/log-gamma.rkt"
          "private/functions/log-arithmetic.rkt"
          "private/functions/incomplete-gamma.rkt"))

;; reciprocal gamma
;; regularized gamma (gamma cdf)

;; inverse gamma
;; inverse regularized gamma (gamma inverse-cdf)

;; beta functions, log beta functions

;; erf
;; erfc
;; inverse erf
;; inverse erfc

;; erf

;; bessel functions

;; psi functions

;; Riemann zeta function, zeta(s)-1

;; arithmetic-geometric mean
