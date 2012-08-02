#lang typed/racket/base

(require "private/array/array-struct.rkt"
         "private/array/array-constructors.rkt"
         "private/array/array-pointwise.rkt"
         "private/array/array-ref.rkt"
         "private/array/array-transform.rkt"
         "private/array/array-convert.rkt"
         "private/array/array-fold.rkt"
         "private/array/array-print.rkt"
         "private/array/array-fft.rkt"
         "private/array/utils.rkt"
         "private/array/array-comprehension.rkt"
         "private/array/array-sequence.rkt")

;; Set the custom printer to a pretty one
(array-custom-printer print-array)

(provide (all-from-out "private/array/array-struct.rkt"
                       "private/array/array-constructors.rkt"
                       "private/array/array-pointwise.rkt"
                       "private/array/array-ref.rkt"
                       "private/array/array-transform.rkt"
                       "private/array/array-convert.rkt"
                       "private/array/array-fold.rkt"
                       "private/array/array-print.rkt"
                       "private/array/array-fft.rkt"
                       "private/array/array-comprehension.rkt"
                       "private/array/array-sequence.rkt")
         ;; from utils.rkt
         Listof* list-shape
         Vectorof* vector-shape)
