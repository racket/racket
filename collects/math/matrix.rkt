#lang typed/racket/base

(require "private/matrix/matrix-arithmetic.rkt"
         "private/matrix/matrix-constructors.rkt"
         "private/matrix/matrix-basic.rkt"
         "private/matrix/matrix-operations.rkt"
         "private/matrix/matrix-comprehension.rkt"
         "private/matrix/matrix-sequences.rkt"
         "private/matrix/matrix-expt.rkt"
         "private/matrix/matrix-types.rkt"
         "private/matrix/utils.rkt")

(provide (all-from-out
          "private/matrix/matrix-arithmetic.rkt"
          "private/matrix/matrix-constructors.rkt"
          "private/matrix/matrix-basic.rkt"
          "private/matrix/matrix-operations.rkt"
          "private/matrix/matrix-comprehension.rkt"
          "private/matrix/matrix-sequences.rkt"
          "private/matrix/matrix-expt.rkt"
          "private/matrix/matrix-types.rkt")
         matrix?)
