#;#;
#<<END
TR opt: vector-sum.rkt 10:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- binary fixnum comp
TR opt: vector-sum.rkt 10:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 10:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 10:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 10:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 10:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 10:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- fixnum bounded expr
TR opt: vector-sum.rkt 11:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 11:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 11:4 (vector-set! v i (sin (exact->inexact i))) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 12:15 sum -- dead else branch
TR opt: vector-sum.rkt 12:15 sum -- dead else branch
TR opt: vector-sum.rkt 12:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- binary fixnum comp
TR opt: vector-sum.rkt 12:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- dead else branch
TR opt: vector-sum.rkt 12:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- dead else branch
TR opt: vector-sum.rkt 12:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- dead else branch
TR opt: vector-sum.rkt 12:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- fixnum bounded expr
TR opt: vector-sum.rkt 14:11 (vector-ref v i) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 14:4 (+ sum (vector-ref v i)) -- binary float
TR opt: vector-sum.rkt 9:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 9:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 9:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 9:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 9:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
END
""
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

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
