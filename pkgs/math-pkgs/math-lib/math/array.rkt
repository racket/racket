#lang racket/base

(require "private/array/array-struct.rkt"
         "private/array/array-constructors.rkt"
         "private/array/array-pointwise.rkt"
         "private/array/array-indexing.rkt"
         "private/array/array-broadcast.rkt"
         "private/array/array-transform.rkt"
         "private/array/array-convert.rkt"
         "private/array/array-fold.rkt"
         "private/array/array-unfold.rkt"
         "private/array/array-print.rkt"
         "private/array/array-fft.rkt"
         "private/array/array-syntax.rkt"
         "private/array/utils.rkt"
         "private/array/array-comprehension.rkt"
         "private/array/array-sequence.rkt"
         "private/array/mutable-array.rkt"
         "private/array/flarray-struct.rkt"
         "private/array/flarray-pointwise.rkt"
         "private/array/fcarray-struct.rkt"
         "private/array/fcarray-pointwise.rkt"
         "private/array/array-parallel.rkt"
         )

;; Set the custom printer to a pretty one
(array-custom-printer print-array)

(provide (all-from-out
          "private/array/array-struct.rkt"
          "private/array/array-constructors.rkt"
          "private/array/array-pointwise.rkt"
          "private/array/array-indexing.rkt"
          "private/array/array-broadcast.rkt"
          "private/array/array-transform.rkt"
          "private/array/array-convert.rkt"
          "private/array/array-fold.rkt"
          "private/array/array-unfold.rkt"
          "private/array/array-print.rkt"
          "private/array/array-syntax.rkt"
          "private/array/array-fft.rkt"
          "private/array/array-comprehension.rkt"
          "private/array/array-sequence.rkt"
          "private/array/mutable-array.rkt"
          "private/array/flarray-struct.rkt"
          "private/array/flarray-pointwise.rkt"
          "private/array/fcarray-struct.rkt"
          "private/array/fcarray-pointwise.rkt"
          "private/array/array-parallel.rkt"
          )
         ;; from utils.rkt
         Listof*
         Vectorof*
         Indexes
         In-Indexes)
