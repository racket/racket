#lang racket/base

(require typed/untyped-utils
         (except-in racket/math sinh cosh tanh)
         (except-in "private/base/base-functions.rkt"
                    sinh cosh tanh
                    asinh acosh atanh)
         "private/base/base-random.rkt")

(require/untyped-contract
 "private/base/base-functions.rkt"
 [sinh  (Number -> Number)]
 [cosh  (Number -> Number)]
 [tanh  (Number -> Number)]
 [asinh  (Number -> Number)]
 [acosh  (Number -> Number)]
 [atanh  (Number -> Number)])

(provide (all-from-out
          racket/math
          "private/base/base-functions.rkt"
          "private/base/base-random.rkt")
         sinh cosh tanh
         asinh acosh atanh)
