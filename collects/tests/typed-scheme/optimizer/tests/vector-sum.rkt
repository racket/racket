#;
(
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 39:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 49:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- binary fixnum
TR opt: vector-sum.rkt 49:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 49:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 49:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- fixnum bounded expr
TR opt: vector-sum.rkt 51:2 (displayln (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 51:2 (displayln (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 51:2 (displayln (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 52:3 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- binary fixnum
TR opt: vector-sum.rkt 52:3 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- fixnum bounded expr
TR opt: vector-sum.rkt 50:4 (vector-set! v i (sin (exact->inexact i))) -- vector access splitting
TR opt: vector-sum.rkt 50:22 sin -- unary float
TR opt: vector-sum.rkt 50:27 exact->inexact -- fixnum to float
TR opt: vector-sum.rkt 52:16 sum -- dead else branch
TR opt: vector-sum.rkt 52:16 sum -- dead else branch
TR opt: vector-sum.rkt 54:6 + -- binary float
TR opt: vector-sum.rkt 54:12 (vector-ref v i) -- vector access splitting
1.53534361535036
1.53534361535036
1.53534361535036
1.53534361535036
1.53534361535036
1.53534361535036
1.53534361535036
1.53534361535036
1.53534361535036
1.53534361535036
)

#lang typed/racket

;; micro-benchmark to measure the effectiveness of partial bounds checking
;; elimination

(define: l : Index 10000000)

(define: v : (Vectorof Float) (make-vector l 0.0))

(for ([i (in-range 10)])
  (for: ([i : Nonnegative-Fixnum (in-range l)])
    (vector-set! v i (sin (exact->inexact i))))
  (displayln
   (for/fold: ([sum : Float 0.0])
       ([i : Nonnegative-Fixnum (in-range l)])
     (+ sum (vector-ref v i)))))
