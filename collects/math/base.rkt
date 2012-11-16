#lang racket/base

(require typed/untyped-utils
         racket/math
         (except-in "private/base/base-functions.rkt"
                    asinh acosh atanh)
         "private/base/base-random.rkt"
         "private/base/base-constants.rkt")

(require/untyped-contract
 "private/base/base-functions.rkt"
 [asinh  (Number -> Number)]
 [acosh  (Number -> Number)]
 [atanh  (Number -> Number)])

(provide (all-from-out
          racket/math
          "private/base/base-functions.rkt"
          "private/base/base-random.rkt"
          "private/base/base-constants.rkt")
         asinh acosh atanh)
