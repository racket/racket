#lang typed/racket/base

(require "bigfloat-struct.rkt")

(provide bfcontinued-fraction)

(define-syntax-rule (bfcontinued-fraction a-init a-fun b-init b-fun eps-expr)
  (bfcopy
   (parameterize ([bf-precision  (+ (bf-precision) 20)])
     (let: ([eps : Bigfloat  eps-expr])
       (let: loop : Bigfloat ([i : Bigfloat 0.bf]
                              [a : Bigfloat a-init]
                              [b : Bigfloat b-init]
                              [last-n : Bigfloat 1.bf]
                              [last-d : Bigfloat 0.bf]
                              [n : Bigfloat 0.bf]
                              [d : Bigfloat 1.bf]
                              [x : Bigfloat 0.bf])
         ;(printf "a = ~v  b = ~v~n" a b)
         (define next-n (bf+ (bf* a last-n) (bf* b n)))
         (define next-d (bf+ (bf* a last-d) (bf* b d)))
         (define next-x (bf/ next-n next-d))
         ;(printf "n = ~v  d = ~v  x = ~v~n" next-n next-d next-x)
         (cond [((bfabs (bf- x next-x)) . bf<= . (bf* eps (bfabs next-x)))
                ;(printf "i = ~v~n" i)
                ;i
                next-x]
               [else
                (let ([i  (bf+ i 1.bf)])
                  (loop i (a-fun i a) (b-fun i b) n d next-n next-d next-x))]))))))
