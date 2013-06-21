#;#;
#<<END
TR opt: vector-sum.rkt 78:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 78:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 78:0 #%module-begin -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- binary fixnum comp
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 89:4 (vector-set! v i (sin (exact->inexact i))) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- fixnum bounded expr
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- binary fixnum comp
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 89:4 (vector-set! v i (sin (exact->inexact i))) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- fixnum bounded expr
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- binary fixnum comp
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 92:11 (vector-ref v i) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 92:4 (+ sum (vector-ref v i)) -- binary float
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- fixnum bounded expr
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- binary fixnum comp
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 92:11 (vector-ref v i) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 92:4 (+ sum (vector-ref v i)) -- binary float
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- fixnum bounded expr
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- binary fixnum comp
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 89:4 (vector-set! v i (sin (exact->inexact i))) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- fixnum bounded expr
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- binary fixnum comp
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 89:4 (vector-set! v i (sin (exact->inexact i))) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 89:26 (exact->inexact i) -- fixnum to float
TR opt: vector-sum.rkt 89:21 (sin (exact->inexact i)) -- unary float
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- dead else branch
TR opt: vector-sum.rkt 88:2 (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) -- fixnum bounded expr
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- binary fixnum comp
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 92:11 (vector-ref v i) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 92:4 (+ sum (vector-ref v i)) -- binary float
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- fixnum bounded expr
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- binary fixnum comp
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 92:11 (vector-ref v i) -- vector partial bounds checking elimination
TR opt: vector-sum.rkt 92:4 (+ sum (vector-ref v i)) -- binary float
TR opt: vector-sum.rkt 90:15 sum -- dead else branch
TR opt: vector-sum.rkt 90:2 (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i))) -- fixnum bounded expr
TR opt: vector-sum.rkt 87:0 (for ((i (in-range 1))) (for: ((i : Nonnegative-Fixnum (in-range l))) (vector-set! v i (sin (exact->inexact i)))) (for/fold: ((sum : Float 0.0)) ((i : Nonnegative-Fixnum (in-range l))) (+ sum (vector-ref v i)))) -- dead else branch

END
""

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
