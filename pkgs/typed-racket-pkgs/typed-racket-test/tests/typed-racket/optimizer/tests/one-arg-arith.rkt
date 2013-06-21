#;#;
#<<END
TR opt: one-arg-arith.rkt 43:0 (- 12) -- unary fixnum
TR opt: one-arg-arith.rkt 44:0 (- 12.0) -- unary float
TR opt: one-arg-arith.rkt 45:0 (/ 4.2) -- unary float
TR opt: one-arg-arith.rkt 47:0 (+ 1) -- unary number
TR opt: one-arg-arith.rkt 48:0 (+ 1.0) -- unary number
TR opt: one-arg-arith.rkt 49:0 (+ (expt 2 100)) -- unary number
TR opt: one-arg-arith.rkt 50:0 (* 1) -- unary number
TR opt: one-arg-arith.rkt 51:0 (* 1.0) -- unary number
TR opt: one-arg-arith.rkt 52:0 (* (expt 2 100)) -- unary number
TR opt: one-arg-arith.rkt 53:0 (min 1) -- unary number
TR opt: one-arg-arith.rkt 54:0 (min 1.0) -- unary number
TR opt: one-arg-arith.rkt 55:0 (min (expt 2 100)) -- unary number
TR opt: one-arg-arith.rkt 56:0 (max 1) -- unary number
TR opt: one-arg-arith.rkt 57:0 (max 1.0) -- unary number
TR opt: one-arg-arith.rkt 58:0 (max (expt 2 100)) -- unary number
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
