#lang racket/base

(require typed/untyped-utils)

(require (except-in "private/functions/gamma.rkt" gamma)
         (except-in "private/functions/log-gamma.rkt" log-gamma)
         (except-in "private/functions/log-factorial.rkt" log-factorial log-binomial log-permutations)
         (except-in "private/functions/beta.rkt" beta log-beta)
         (except-in "private/functions/erf.rkt" erf erfc)
         (except-in "private/functions/lambert.rkt" lambert)
         "private/functions/incomplete-gamma.rkt"
         "private/functions/incomplete-beta.rkt"
         "private/functions/gammastar.rkt")

(require/untyped-contract
 "private/functions/gamma.rkt"
 [gamma  (Real -> Real)])

(require/untyped-contract
 "private/functions/log-gamma.rkt"
 [log-gamma  (Real -> Real)])

(require/untyped-contract
 "private/functions/log-factorial.rkt"
 [log-factorial  (Integer -> Real)]
 [log-binomial  (Integer Integer -> Real)]
 [log-permutations  (Integer Integer -> Real)])

(require/untyped-contract
 "private/functions/beta.rkt"
 [beta  (Real Real -> Real)]
 [log-beta  (Real Real -> Real)])

(require/untyped-contract
 "private/functions/erf.rkt"
 [erf  (Real -> Real)]
 [erfc  (Real -> Real)])

(require/untyped-contract
 "private/functions/lambert.rkt"
 [lambert  (Real -> Real)])

(provide (all-from-out
          "private/functions/gamma.rkt"
          "private/functions/log-gamma.rkt"
          "private/functions/log-factorial.rkt"
          "private/functions/beta.rkt"
          "private/functions/erf.rkt"
          "private/functions/lambert.rkt"
          "private/functions/incomplete-gamma.rkt"
          "private/functions/incomplete-beta.rkt"
          "private/functions/gammastar.rkt")
         gamma
         log-gamma
         log-factorial log-binomial log-permutations
         beta log-beta
         erf erfc
         lambert
         )
