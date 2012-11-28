#lang racket/base

(require (for-syntax racket/base)
         typed/untyped-utils
         racket/math
         (rename-in
          (except-in "private/base/base-functions.rkt"
                     asinh acosh atanh)
          [number->float-complex  typed:number->float-complex])
         "private/base/base-random.rkt"
         "private/base/base-constants.rkt")

(require/untyped-contract
 "private/base/base-functions.rkt"
 [asinh  (Number -> Number)]
 [acosh  (Number -> Number)]
 [atanh  (Number -> Number)])

(define-syntax (number->float-complex stx)
  (syntax-case stx ()
    [(_ z-expr)  (syntax/loc stx (inline-number->float-complex z-expr))]
    [(_ . args)  (syntax/loc stx (typed:number->float-complex . args))]
    [_  (syntax/loc stx typed:number->float-complex)]))

(provide (all-from-out
          racket/math
          "private/base/base-functions.rkt"
          "private/base/base-random.rkt"
          "private/base/base-constants.rkt")
         asinh acosh atanh
         number->float-complex)
