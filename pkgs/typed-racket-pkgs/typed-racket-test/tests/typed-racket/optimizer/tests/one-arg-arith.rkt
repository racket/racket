#;#;
#<<END
TR opt: one-arg-arith.rkt 10:0 (+ (expt 2 100)) -- unary number
TR opt: one-arg-arith.rkt 11:0 (* 1) -- unary number
TR opt: one-arg-arith.rkt 12:0 (* 1.0) -- unary number
TR opt: one-arg-arith.rkt 13:0 (* (expt 2 100)) -- unary number
TR opt: one-arg-arith.rkt 14:0 (min 1) -- unary number
TR opt: one-arg-arith.rkt 15:0 (min 1.0) -- unary number
TR opt: one-arg-arith.rkt 16:0 (min (expt 2 100)) -- unary number
TR opt: one-arg-arith.rkt 17:0 (max 1) -- unary number
TR opt: one-arg-arith.rkt 18:0 (max 1.0) -- unary number
TR opt: one-arg-arith.rkt 19:0 (max (expt 2 100)) -- unary number
TR opt: one-arg-arith.rkt 4:0 (- 12) -- unary fixnum
TR opt: one-arg-arith.rkt 5:0 (- 12.0) -- unary float
TR opt: one-arg-arith.rkt 6:0 (/ 4.2) -- unary float
TR opt: one-arg-arith.rkt 8:0 (+ 1) -- unary number
TR opt: one-arg-arith.rkt 9:0 (+ 1.0) -- unary number
END
#<<END
-12
-12.0
0.23809523809523808
1
1.0
1267650600228229401496703205376
1
1.0
1267650600228229401496703205376
1
1.0
1267650600228229401496703205376
1
1.0
1267650600228229401496703205376

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port



(- 12)
(- 12.0)
(/ 4.2)

(+ 1)
(+ 1.0)
(+ (expt 2 100))
(* 1)
(* 1.0)
(* (expt 2 100))
(min 1)
(min 1.0)
(min (expt 2 100))
(max 1)
(max 1.0)
(max (expt 2 100))
