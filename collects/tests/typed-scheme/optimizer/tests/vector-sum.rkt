#;
(
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 29:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- binary fixnum
TR opt: vector-sum.rkt 39:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 39:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 39:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- fixnum bounded expr
TR opt: vector-sum.rkt 41:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- binary fixnum
TR opt: vector-sum.rkt 41:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- fixnum bounded expr
TR opt: vector-sum.rkt 40:4 (vector-set! v i (sin (exact->inexact i))) -- vector access splitting
TR opt: vector-sum.rkt 40:22 sin -- unary float
TR opt: vector-sum.rkt 40:27 exact->inexact -- fixnum to float
TR opt: vector-sum.rkt 41:15 sum -- dead else branch
TR opt: vector-sum.rkt 41:15 sum -- dead else branch
TR opt: vector-sum.rkt 43:5 + -- binary float
TR opt: vector-sum.rkt 43:11 (vector-ref v i) -- vector access splitting
)

#lang typed/racket

;; micro-benchmark to measure the effectiveness of partial bounds checking
;; elimination

(define: l : Index 10000000)

(define: v : (Vectorof Float) (make-vector l 0.0))

(for ([i (in-range 1)])
  (for: ([i : Nonnegative-Fixnum (in-range l)])
    (vector-set! v i (sin (exact->inexact i))))
  (for/fold: ([sum : Float 0.0])
      ([i : Nonnegative-Fixnum (in-range l)])
    (+ sum (vector-ref v i))))
