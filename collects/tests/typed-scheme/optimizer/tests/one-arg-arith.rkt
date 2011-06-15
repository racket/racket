#;
(
TR opt: one-arg-arith.rkt 40:1 - -- unary fixnum
TR opt: one-arg-arith.rkt 41:1 - -- unary float
TR opt: one-arg-arith.rkt 42:1 / -- unary float
TR opt: one-arg-arith.rkt 44:1 + -- unary number
TR opt: one-arg-arith.rkt 45:1 + -- unary number
TR opt: one-arg-arith.rkt 46:1 + -- unary number
TR opt: one-arg-arith.rkt 47:1 * -- unary number
TR opt: one-arg-arith.rkt 48:1 * -- unary number
TR opt: one-arg-arith.rkt 49:1 * -- unary number
TR opt: one-arg-arith.rkt 50:1 min -- unary number
TR opt: one-arg-arith.rkt 51:1 min -- unary number
TR opt: one-arg-arith.rkt 52:1 min -- unary number
TR opt: one-arg-arith.rkt 53:1 max -- unary number
TR opt: one-arg-arith.rkt 54:1 max -- unary number
TR opt: one-arg-arith.rkt 55:1 max -- unary number
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
)

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
