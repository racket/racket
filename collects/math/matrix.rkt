#lang typed/racket/base

(require "private/matrix/matrix-pointwise.rkt"
         "private/matrix/matrix-multiply.rkt"
         "private/matrix/matrix-constructors.rkt"
         "private/matrix/matrix-operations.rkt"
         "private/matrix/matrix-expt.rkt"
         "private/matrix/matrix-types.rkt"
         "private/matrix/utils.rkt")

(provide (all-from-out "private/matrix/matrix-pointwise.rkt"
                       "private/matrix/matrix-multiply.rkt"
                       "private/matrix/matrix-constructors.rkt"
                       "private/matrix/matrix-operations.rkt"
                       "private/matrix/matrix-expt.rkt"
                       "private/matrix/matrix-types.rkt")
         ;; From "utils.rkt"
         array-matrix?
         ;; would also like matrix? : (Any -> Boolean : (Array Any)), but we can't have one until we
         ;; can define array? : (Any -> Boolean : (Array Any)), and there's been trouble with that
         )
