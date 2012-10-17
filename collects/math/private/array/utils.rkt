#lang racket/base

(require typed/untyped-utils
         typed/racket/base
         (except-in "typed-utils.rkt"
                    check-array-shape))

(require/untyped-contract
 "typed-utils.rkt"
 [check-array-shape  ((Vectorof Integer) (-> Nothing) -> (Vectorof Index))])

(provide (all-from-out "typed-utils.rkt")
         check-array-shape)
