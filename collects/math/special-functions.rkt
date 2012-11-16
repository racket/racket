#lang racket/base

(require typed/untyped-utils)

(require (except-in "private/functions/gamma.rkt" gamma)
         (except-in "private/functions/log-gamma.rkt" log-gamma)
         (except-in "private/functions/beta.rkt" beta log-beta)
         (except-in "private/functions/erf.rkt" erf erfc)
         (except-in "private/functions/lambert.rkt" lambert)
         (except-in "private/functions/zeta.rkt" eta zeta)
         (except-in "private/functions/hurwitz-zeta.rkt" hurwitz-zeta)
         "private/functions/psi.rkt"
         "private/functions/incomplete-gamma.rkt"
         "private/functions/incomplete-beta.rkt"
         "private/functions/stirling-error.rkt")

(require/untyped-contract
 "private/functions/gamma.rkt"
 [gamma  (Real -> Real)])

(require/untyped-contract
 "private/functions/log-gamma.rkt"
 [log-gamma  (Real -> Real)])

(require/untyped-contract
 "private/functions/beta.rkt"
 [beta  (Real Real -> Real)]
 [log-beta  (Real Real -> Real)])

(require/untyped-contract
 "private/functions/erf.rkt"
 [erf   (Real -> Real)]
 [erfc  (Real -> Real)])

(require/untyped-contract
 "private/functions/lambert.rkt"
 [lambert  (Real -> Real)])

(require/untyped-contract
 "private/functions/zeta.rkt"
 [eta   (Real -> Real)]
 [zeta  (Real -> Real)])

(require/untyped-contract
 "private/functions/hurwitz-zeta.rkt"
 [hurwitz-zeta  (Real Real -> Real)])

(provide (all-from-out
          "private/functions/gamma.rkt"
          "private/functions/log-gamma.rkt"
          "private/functions/beta.rkt"
          "private/functions/erf.rkt"
          "private/functions/lambert.rkt"
          "private/functions/zeta.rkt"
          "private/functions/hurwitz-zeta.rkt"
          "private/functions/psi.rkt"
          "private/functions/incomplete-gamma.rkt"
          "private/functions/incomplete-beta.rkt"
          "private/functions/stirling-error.rkt")
         gamma
         log-gamma
         beta log-beta
         erf erfc
         lambert
         eta zeta
         hurwitz-zeta
         )
