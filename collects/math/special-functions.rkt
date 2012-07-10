#lang typed/racket/base

(require "private/log1p.rkt"
         "private/expm1.rkt"
         "private/gamma.rkt"
         "private/log-factorial.rkt"
         "private/log-gamma.rkt")

(provide (all-from-out "private/log1p.rkt"
                       "private/expm1.rkt"
                       "private/gamma.rkt"
                       "private/log-factorial.rkt"
                       "private/log-gamma.rkt"))

;; reciprocal gamma
;; upper and lower gamma
;; regularized gamma (gamma cdf)

;; inverse gamma
;; inverse regularized gamma (gamma cdf)

;; beta functions, log beta functions

;; erf
;; erfc
;; inverse erf
;; inverse erfc

;; log erf?

;; bessel functions

;; psi functions

;; Riemann zeta function, zeta(s)-1

;; arithmetic-geometric mean
