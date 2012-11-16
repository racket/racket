#lang racket/base

(require typed/untyped-utils
         (rename-in
          (only-in "typed-array-pointwise.rkt"
                   array-map
                   array-sqrt
                   array-log
                   array<
                   array<=
                   array>
                   array>=
                   array=
                   array-not
                   array-and
                   array-or
                   array-if)
          [array-map typed:array-map])
         (rename-in "untyped-array-pointwise.rkt"
                    [array-map  untyped:array-map]))

(define-typed/untyped-identifier array-map
  typed:array-map untyped:array-map)

(require/untyped-contract
 (begin (require "array-struct.rkt"))
 "typed-array-pointwise.rkt"
 [array-abs  ((Array Real) -> (Array Real))]
 [array-round  ((Array Real) -> (Array Real))]
 [array-floor  ((Array Real) -> (Array Real))]
 [array-ceiling  ((Array Real) -> (Array Real))]
 [array-truncate  ((Array Real) -> (Array Real))]
 [array-conjugate  ((Array Number) -> (Array Number))]
 [array-magnitude  ((Array Number) -> (Array Real))]
 [array-angle  ((Array Number) -> (Array Real))]
 [array-sqr   ((Array Number) -> (Array Number))]
 [array-exp   ((Array Number) -> (Array Number))]
 [array-sin   ((Array Number) -> (Array Number))]
 [array-cos   ((Array Number) -> (Array Number))]
 [array-tan   ((Array Number) -> (Array Number))]
 [array-asin  ((Array Number) -> (Array Number))]
 [array-acos  ((Array Number) -> (Array Number))]
 [array-atan  ((Array Number) -> (Array Number))]
 [array+  ((Array Number) (Array Number) -> (Array Number))]
 [array*  ((Array Number) (Array Number) -> (Array Number))]
 [array-  (case-> ((Array Number) -> (Array Number))
                  ((Array Number) (Array Number) -> (Array Number)))]
 [array/  (case-> ((Array Number) -> (Array Number))
                  ((Array Number) (Array Number) -> (Array Number)))]
 [array-scale  ((Array Number) Number -> (Array Number))]
 [array-expt  ((Array Number) (Array Number) -> (Array Number))]
 [array-min  ((Array Real) (Array Real) -> (Array Real))]
 [array-max  ((Array Real) (Array Real) -> (Array Real))]
 [array-inexact->exact  ((Array Number) -> (Array Exact-Number))]
 [array-exact->inexact  ((Array Number) -> (Array Number))]  ; should be Number -> Inexact-Number
 [array-real->double-flonum  ((Array Real) -> (Array Float))]
 [array-number->float-complex  ((Array Number) -> (Array Float-Complex))]
 [array-real-part  ((Array Number) -> (Array Real))]
 [array-imag-part  ((Array Number) -> (Array Real))]
 [array-make-rectangular  ((Array Real) (Array Real) -> (Array Number))])

(provide
 ;; Mapping
 inline-array-map
 array-map
 ;; Lifted operators
 array-scale
 array-abs
 array-round
 array-floor
 array-ceiling
 array-truncate
 array-sqr
 array-sqrt
 array-conjugate
 array-magnitude
 array-angle
 array-log
 array-exp
 array-sin
 array-cos
 array-tan
 array-asin
 array-acos
 array-atan
 array+
 array-
 array*
 array/
 array-expt
 array-min
 array-max     
 array=
 array<
 array<=
 array>
 array>=
 array-not
 array-and
 array-or
 array-if
 ;; Number conversions
 array-inexact->exact
 array-exact->inexact
 array-real->double-flonum
 array-number->float-complex
 array-real-part
 array-imag-part
 array-make-rectangular)
