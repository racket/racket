#lang racket/base

(require typed/untyped-utils
         (except-in "typed-array-fold.rkt"
                    array-axis-sum
                    array-axis-prod
                    array-axis-min
                    array-axis-max
                    array-all-sum
                    array-all-prod
                    array-all-min
                    array-all-max))

(require/untyped-contract
 (begin (require "array-struct.rkt"))
 "typed-array-fold.rkt"
 [array-axis-sum  (case-> ((Array Number) Integer -> (Array Number))
                          ((Array Number) Integer Number -> (Array Number)))]
 [array-axis-prod  (case-> ((Array Number) Integer -> (Array Number))
                           ((Array Number) Integer Number -> (Array Number)))]
 [array-axis-min  (case-> ((Array Real) Integer -> (Array Real))
                          ((Array Real) Integer Real -> (Array Real)))]
 [array-axis-max  (case-> ((Array Real) Integer -> (Array Real))
                          ((Array Real) Integer Real -> (Array Real)))]
 [array-all-sum  (case-> ((Array Number) -> Number)
                         ((Array Number) Number -> Number))]
 [array-all-prod  (case-> ((Array Number) -> Number)
                          ((Array Number) Number -> Number))]
 [array-all-min  (case-> ((Array Real) -> Real)
                         ((Array Real) Real -> Real))]
 [array-all-max  (case-> ((Array Real) -> Real)
                         ((Array Real) Real -> Real))])

(provide array-axis-fold
         array-axis-sum
         array-axis-prod
         array-axis-min
         array-axis-max
         array-axis-count
         array-axis-andmap
         array-axis-ormap
         array-fold
         array-all-sum
         array-all-prod
         array-all-min
         array-all-max
         array-all-count
         array-all-andmap
         array-all-ormap
         array-all-equal?
         array-all-eqv?
         array-all-eq?
         array-all=)
